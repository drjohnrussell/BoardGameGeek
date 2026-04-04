library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(ggplot2)
library(treemapify)

# ── 1. Load data and select top 500 non-expansion games ──────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(rank), rank > 0) |>
  arrange(rank) |>
  slice_head(n = 500)

# ── 2. Fetch mechanics from BGG API ──────────────────────────────────────────
# BGG allows comma-separated IDs in one request; batches of 20 keeps us safe
base_url <- "https://boardgamegeek.com/xmlapi2"

fetch_mechanics <- function(ids) {
  resp <- request(base_url) |>
    req_url_path_append("thing") |>
    req_url_query(id = paste(ids, collapse = ","), type = "boardgame") |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_auth_bearer_token(Sys.getenv("BGG_oauth_token")) |>
    req_retry(max_tries = 3, backoff = \(x) 30) |>
    req_perform()

  xml <- resp_body_xml(resp)
  items <- xml_find_all(xml, "//item")

  map_dfr(items, function(item) {
    game_id  <- xml_attr(item, "id")
    links    <- xml_find_all(item, ".//link[@type='boardgamemechanic']")
    mechanics <- xml_attr(links, "value")

    if (length(mechanics) == 0) {
      tibble(game_id = game_id, mechanic = NA_character_)
    } else {
      tibble(game_id = game_id, mechanic = mechanics)
    }
  })
}

# Split IDs into batches of 20 and fetch, pausing between each
batches <- split(games$id, ceiling(seq_along(games$id) / 20))

cache_file <- "mechanics_raw.rds"

if (file.exists(cache_file)) {
  message("Loading cached mechanics data...")
  mechanics_raw <- readRDS(cache_file)
} else {
  mechanics_raw <- map_dfr(batches, function(batch) {
    message("Fetching batch of ", length(batch), " games...")
    result <- fetch_mechanics(batch)
    Sys.sleep(2)   # be respectful of BGG's servers
    result
  })
  saveRDS(mechanics_raw, cache_file)
}

## put in the board games for exploration
mechanics_raw <- mechanics_raw |>
  left_join(games |> 
    mutate(id=as.character(id)) |> 
      select(id, name), by = c("game_id" = "id"))

# ── 3. Aggregate mechanic counts ─────────────────────────────────────────────
mechanic_counts <- mechanics_raw |>
  filter(!is.na(mechanic)) |>
  count(mechanic, name = "n_games") |>
  arrange(desc(n_games))

# Keep only mechanics appearing in at least 20 games
plot_data <- mechanic_counts |>
  filter(n_games >= 20) |>
  mutate(label = paste0(mechanic, "\n", n_games, " games"))

# ── 4. Plot ───────────────────────────────────────────────────────────────────
ggplot(plot_data,
       aes(area  = n_games,
           fill  = n_games,
           label = label)) +
  geom_treemap() +
  geom_treemap_text(
    colour  = "white",
    place   = "centre",
    grow    = FALSE,
    reflow  = TRUE,
    size    = 10
  ) +
  scale_fill_viridis_c(option = "turbo", direction = -1, name = "# games") +
  labs(
    title    = "What mechanics define BGG's top 500 board games?",
    subtitle = "Only mechanics used by 20 or more of the top 500 games are shown.\nGames typically have multiple mechanics; tiles do not sum to 500.",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 1: Part-to-Whole"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    plot.caption  = element_text(colour = "grey60", size = 8),
    legend.position = "right"
  )

ggsave("images/day01_mechanics_treemap.png", width = 12, height = 8, dpi = 300)
