library(tidyverse)
library(scales)
library(showtext)
library(httr2)
library(xml2)

font_add_google("Libre Baskerville", "baskerville")
font_add_google("Source Sans 3",     "sourcesans")
showtext_auto()

# Load token from .Renviron if not already set (e.g. when run from bash)
if (!nzchar(Sys.getenv("BGG_oauth_token"))) readRenviron("~/.Renviron")
bgg_token <- Sys.getenv("BGG_oauth_token")

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0)

top500 <- ranks |> arrange(rank) |> slice_head(n = 500)

# ── API fetch: suggested_numplayers poll for top 500 games ────────────────────
cache_file <- "player_counts_raw.rds"

if (!file.exists(cache_file)) {
  ids     <- top500$id
  batches <- split(ids, ceiling(seq_along(ids) / 20))

  player_data <- map_dfr(seq_along(batches), function(i) {
    resp <- request("https://boardgamegeek.com/xmlapi2/thing") |>
      req_url_query(id = paste(batches[[i]], collapse = ","), type = "boardgame") |>
      req_auth_bearer_token(bgg_token) |>
      req_perform() |>
      resp_body_xml()

    items <- xml_find_all(resp, "/items/item[@type='boardgame']")

    batch_df <- map_dfr(items, function(item) {
      game_id <- xml_attr(item, "id")
      poll    <- xml_find_first(item, ".//poll[@name='suggested_numplayers']")
      if (is.na(poll)) return(NULL)

      map_dfr(xml_find_all(poll, ".//results"), function(rn) {
        result_nodes <- xml_find_all(rn, ".//result")
        tibble(
          game_id    = game_id,
          numplayers = xml_attr(rn, "numplayers"),
          vote_type  = xml_attr(result_nodes, "value"),
          numvotes   = as.integer(xml_attr(result_nodes, "numvotes"))
        )
      })
    })

    message("Batch ", i, "/", length(batches), " done")
    if (i < length(batches)) Sys.sleep(2)
    batch_df
  })

  saveRDS(player_data, cache_file)
} else {
  player_data <- readRDS(cache_file)
}

# ── Heatmap data: % voting "Best" per game × player count ────────────────────
# Keep only pure integer player counts 1–8
pct_best <- player_data |>
  filter(str_detect(numplayers, "^\\d+$")) |>
  mutate(numplayers = as.integer(numplayers)) |>
  filter(numplayers <= 8) |>
  group_by(game_id, numplayers) |>
  summarise(
    best  = sum(numvotes[vote_type == "Best"],  na.rm = TRUE),
    total = sum(numvotes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total >= 5) |>
  mutate(pct_best = best / total)

# Top 50 for plot, labelled with rank
top50 <- top500 |>
  slice_head(n = 50) |>
  mutate(
    game_id    = as.character(id),
    label_name = paste0("#", rank, "  ", name)
  )

plot_data <- top50 |>
  left_join(pct_best, by = "game_id") |>
  filter(!is.na(numplayers), numplayers <= 6) |>
  mutate(
    label_name = factor(label_name, levels = rev(top50$label_name))
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(plot_data, aes(x = numplayers, y = label_name, fill = pct_best)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_x_continuous(
    breaks = 1:6,
    labels = as.character(1:6),
    expand = expansion(add = 0.5),
    position = "top"
  ) +
  scale_fill_gradient(
    low      = "#D6E4F5",
    high     = "#0D2B5E",
    na.value = "grey93",
    limits   = c(0, 1),
    labels   = percent_format(accuracy = 1),
    name     = "% voting\n'Best'"
  ) +
  labs(
    title    = "Finding the Sweet Spot",
    subtitle = "Share of BGG voters rating each player count as 'Best' for the top 50 ranked games.\nDarker = stronger consensus. Grey = fewer than 5 votes cast for that count.",
    x        = "Player count",
    y        = NULL,
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 29: Monochrome"
  ) +
  theme_minimal(base_family = "sourcesans", base_size = 20) +
  theme(
    plot.title    = element_text(family = "baskerville", size = 45, face = "bold",
                                 margin = margin(b = 6)),
    plot.subtitle = element_text(size = 26, colour = "#444444",
                                 lineheight = 0.45, margin = margin(b = 10)),
    plot.caption  = element_text(size = 15, colour = "#888888", margin = margin(t = 12)),
    plot.margin   = margin(18, 20, 12, 18),
    panel.grid    = element_blank(),
    axis.text.y   = element_text(size = 18, colour = "#333333", hjust = 1),
    axis.text.x   = element_text(size = 20, colour = "#444444", face = "bold"),
    axis.title.x  = element_blank(),
    legend.title  = element_text(size = 18, lineheight = 0.4),
    legend.text   = element_text(size = 16)
  )

ggsave("images/day29_monochrome.png", p, width = 13, height = 15, dpi = 300)
