library(httr2)
library(xml2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggrepel)

# ── 1. Load top 1000 non-expansion ranked games ───────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 1000)

# ── 2. Fetch mechanics from BGG API (cached) ──────────────────────────────────
base_url <- "https://boardgamegeek.com/xmlapi2"

fetch_mechanics <- function(ids) {
  resp <- request(base_url) |>
    req_url_path_append("thing") |>
    req_url_query(id = paste(ids, collapse = ","), type = "boardgame") |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_auth_bearer_token(Sys.getenv("BGG_oauth_token")) |>
    req_retry(max_tries = 3, backoff = \(x) 30) |>
    req_perform()

  xml  <- resp_body_xml(resp)
  items <- xml_find_all(xml, "/items/item[@type='boardgame']")

  map_dfr(items, function(item) {
    game_id   <- xml_attr(item, "id")
    links     <- xml_find_all(item, ".//link[@type='boardgamemechanic']")
    mechanics <- xml_attr(links, "value")

    if (length(mechanics) == 0) {
      tibble(game_id = game_id, mechanic = NA_character_)
    } else {
      tibble(game_id = game_id, mechanic = mechanics)
    }
  })
}

cache_file <- "mechanics_raw_1000.rds"

if (file.exists(cache_file)) {
  message("Loading cached mechanics data (top 1000)...")
  mechanics_raw <- readRDS(cache_file)
} else {
  batches <- split(games$id, ceiling(seq_along(games$id) / 20))
  mechanics_raw <- map_dfr(batches, function(batch) {
    message("Fetching batch of ", length(batch), " games...")
    result <- fetch_mechanics(batch)
    Sys.sleep(2)
    result
  })
  saveRDS(mechanics_raw, cache_file)
}

# ── 3. Join with year published ───────────────────────────────────────────────
mech <- mechanics_raw |>
  filter(!is.na(mechanic)) |>
  left_join(
    games |> mutate(id = as.character(id)) |> select(id, yearpublished),
    by = c("game_id" = "id")
  ) |>
  filter(!is.na(yearpublished), yearpublished >= 1990, yearpublished <= 2024)

# ── 4. Pick 7 mechanics with the most interesting stories ────────────────────
# Hand Management & Dice Rolling = long-time staples; Worker Placement & Deck
# Building = modern era risers; Cooperative & Variable Player Powers = steady
# climbers; Set Collection = old-school mechanic on the decline
top_mechs <- c(
  "Hand Management",
  "Dice Rolling",
  "Worker Placement",
  "Deck, Bag, and Pool Building",
  "Cooperative Game",
  "Variable Player Powers",
  "Set Collection"
)

short <- c(
  "Hand Management"              = "Hand Management",
  "Dice Rolling"                 = "Dice Rolling",
  "Worker Placement"             = "Worker Placement",
  "Deck, Bag, and Pool Building" = "Deck Building",
  "Cooperative Game"             = "Cooperative",
  "Variable Player Powers"       = "Variable Player Powers",
  "Set Collection"               = "Set Collection"
)
label_fn <- function(x) ifelse(x %in% names(short), short[x], x)

# ── 5. Compute per-year share for each mechanic ───────────────────────────────
games_per_year <- games |>
  filter(yearpublished >= 1990, yearpublished <= 2024) |>
  count(yearpublished, name = "total_games")

mech_share <- mech |>
  filter(mechanic %in% top_mechs) |>
  count(yearpublished, mechanic, name = "n_games") |>
  left_join(games_per_year, by = "yearpublished") |>
  filter(total_games >= 10) |>
  mutate(
    share      = n_games / total_games,
    mech_short = label_fn(mechanic)
  )

# ── 6. Label positions at 2024 ────────────────────────────────────────────────
label_right <- mech_share |> filter(yearpublished == 2024)

# ── 7. Palette ────────────────────────────────────────────────────────────────
pal <- c(
  "Hand Management"              = "#4E79A7",
  "Dice Rolling"                 = "#BAB0AC",
  "Worker Placement"             = "#F28E2B",
  "Deck, Bag, and Pool Building" = "#E15759",
  "Cooperative Game"             = "#59A14F",
  "Variable Player Powers"       = "#B07AA1",
  "Set Collection"               = "#76B7B2"
)

# ── 8. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(mech_share, aes(x = yearpublished, y = share,
                             color = mechanic, group = mechanic)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_smooth(se = FALSE, linewidth = 1.4, span = 0.55, method = "loess") +
  geom_text(
    data  = label_right,
    aes(label = mech_short),
    hjust = -0.1, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    breaks = seq(1990, 2024, by = 5),
    limits = c(1990, 2036)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "The shifting DNA of BGG's top-ranked board games",
    subtitle = "Share of top-1000 games published each year that feature each mechanic (1990–2024).\nSmoothed loess curves; faint points show raw annual values.",
    x        = NULL,
    y        = "Share of that year's top-1000 games",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 20: Global Change"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#FAFAF7", color = NA),
    panel.grid.major = element_line(color = "grey88"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey60", size = 8),
    plot.margin      = margin(12, 150, 12, 12),
    legend.position  = "none"
  )

ggsave("images/day20_globalchange.png", p, width = 13, height = 8, dpi = 300)
message("Saved \u2192 images/day20_globalchange.png")
