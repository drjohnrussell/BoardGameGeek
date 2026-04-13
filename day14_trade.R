library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggrepel)
library(scales)

# Load .Renviron if token not already in environment
if (!nzchar(Sys.getenv("BGG_oauth_token"))) readRenviron("~/.Renviron")

base_url  <- "https://boardgamegeek.com/xmlapi2"
bgg_token <- Sys.getenv("BGG_oauth_token")

# ── 1. Load top 500 ranked games ───────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 500)

# ── 2. Fetch or load cached designer data ─────────────────────────────────────
cache_file <- "designers_raw.rds"

fetch_designers <- function(ids) {
  req <- request(base_url) |>
    req_url_path_append("thing") |>
    req_url_query(id = paste(ids, collapse = ","), type = "boardgame") |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_retry(max_tries = 3, backoff = \(x) 30)
  if (nzchar(bgg_token)) req <- req_auth_bearer_token(req, bgg_token)
  req |>
    req_perform() |>
    resp_body_xml() |>
    xml_find_all("/items/item[@type='boardgame']") |>
    map_dfr(\(item) {
      game_id  <- xml_attr(item, "id")
      links    <- xml_find_all(item, ".//link[@type='boardgamedesigner']")
      des_ids  <- xml_attr(links, "id")
      des_names <- xml_attr(links, "value")
      if (length(des_ids) == 0) {
        tibble(game_id = game_id, designer_id = NA_character_,
               designer = NA_character_)
      } else {
        tibble(game_id = game_id, designer_id = des_ids, designer = des_names)
      }
    })
}

if (file.exists(cache_file)) {
  message("Loading cached designer data...")
  designers_raw <- readRDS(cache_file)
} else {
  message("Fetching designer data from BGG API...")
  batches <- split(games$id, ceiling(seq_along(games$id) / 20))
  designers_raw <- map_dfr(batches, \(batch) {
    message("  Fetching batch of ", length(batch), "...")
    res <- fetch_designers(batch)
    Sys.sleep(2)
    res
  })
  saveRDS(designers_raw, cache_file)
}

# ── 3. Filter out "uncredited" designer (id 3) and anonyms ────────────────────
designers_clean <- designers_raw |>
  filter(!is.na(designer_id), designer_id != "3") |>
  mutate(game_id = as.integer(game_id),
         designer_id = as.integer(designer_id))

# ── 4. Keep only designers with >= 2 games in the top 500 ─────────────────────
prolific <- designers_clean |>
  count(designer_id, designer, name = "n_games") |>
  filter(n_games >= 2) |>
  arrange(desc(n_games))

des_games <- designers_clean |>
  semi_join(prolific, by = "designer_id")

# ── 5. Build edge list — designers share an edge if they co-designed a game ───
edges <- des_games |>
  inner_join(des_games, by = "game_id", relationship = "many-to-many") |>
  filter(designer_id.x < designer_id.y) |>          # avoid duplicates
  count(designer_id.x, designer.x, designer_id.y, designer.y,
        name = "shared_games") |>
  select(from = designer.x, to = designer.y, shared_games)

# ── 6. Build igraph object ─────────────────────────────────────────────────────
g <- graph_from_data_frame(edges, directed = FALSE,
                            vertices = prolific |> select(designer, n_games) |>
                              rename(name = designer))

# Remove isolates (designers with no collaborations in this set)
g <- delete_vertices(g, V(g)[degree(g) == 0])

message("Nodes: ", vcount(g), "  Edges: ", ecount(g))

# ── 7. Plot ────────────────────────────────────────────────────────────────────
set.seed(42)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = shared_games, alpha = shared_games),
                 colour = "grey60") +
  geom_node_point(aes(size = n_games, colour = n_games)) +
  geom_node_text(
    aes(label = ifelse(n_games >= 5, name, "")),
    repel         = TRUE,
    size          = 2.8,
    colour        = "grey15",
    box.padding   = 0.5,
    point.padding = 0.3,
    segment.colour = "grey60",
    segment.size  = 0.3,
    max.overlaps  = Inf
  ) +
  scale_edge_width(range = c(0.4, 2.5), guide = "none") +
  scale_edge_alpha(range = c(0.3, 0.9), guide = "none") +
  scale_size_continuous(range = c(2, 9), name = "Games in\ntop 500") +
  scale_colour_viridis_c(option = "magma", direction = -1,
                          name = "Games in\ntop 500", end = 0.85) +
  guides(size = guide_legend(override.aes = list(colour = "grey40")),
         colour = "none") +
  labs(
    title    = "The Trade of Game Design",
    subtitle = paste0(
      "Designers of the top 500 BGG-ranked games, connected when they co-designed a title.\n",
      "Node size = number of top-500 games. Labels shown for designers with \u22655 games."
    ),
    caption = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 14: Trade"
  ) +
  theme_graph(base_family = "sans", background = "grey98") +
  theme(
    plot.title      = element_text(face = "bold", size = 20, colour = "grey10",
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(colour = "grey40", size = 10, lineheight = 1.3,
                                   margin = margin(b = 10)),
    plot.caption    = element_text(colour = "grey55", size = 8, hjust = 1),
    legend.position = "right",
    plot.margin     = margin(15, 15, 15, 15)
  )

ggsave("images/day14_trade.png", width = 12, height = 10, dpi = 300)
message("Saved images/day14_trade.png")
