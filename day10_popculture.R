library(httr2)
library(xml2)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)
library(purrr)

# Load .Renviron if token not already in environment (e.g. when run from bash)
if (!nzchar(Sys.getenv("BGG_oauth_token"))) readRenviron("~/.Renviron")

base_url  <- "https://boardgamegeek.com/xmlapi2"
bgg_token <- Sys.getenv("BGG_oauth_token")

# ── 1. Load games ──────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(usersrated), usersrated > 0) |>
  arrange(desc(usersrated)) |>
  slice_head(n = 1000)

# ── 2. Fetch or load cached family tags ────────────────────────────────────────
cache_file <- "families_raw.rds"

fetch_families <- function(ids) {
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
      game_id <- xml_attr(item, "id")
      links   <- xml_find_all(item, ".//link[@type='boardgamefamily']")
      families <- xml_attr(links, "value")
      if (length(families) == 0) {
        tibble(game_id = game_id, family = NA_character_)
      } else {
        tibble(game_id = game_id, family = families)
      }
    })
}

if (file.exists(cache_file)) {
  message("Loading cached family data...")
  families_raw <- readRDS(cache_file)
} else {
  message("Cache not found — fetching from BGG API...")
  batches <- split(games$id, ceiling(seq_along(games$id) / 20))
  families_raw <- map_dfr(batches, \(batch) {
    message("Fetching ", length(batch), " games...")
    res <- fetch_families(batch)
    Sys.sleep(2)
    res
  })
  saveRDS(families_raw, cache_file)
}

families_raw <- families_raw |>
  mutate(game_id = as.integer(game_id))

# ── 3. Parse medium prefix and series name ─────────────────────────────────────
# BGG family tags follow the pattern "Medium: Series Name"
# We want: Movies, Books, TV Shows, Comic Books, Comic Strips, Authors
# Authors (e.g. "Authors: J.R.R. Tolkien") are treated as Books in the legend
pop_tagged <- families_raw |>
  filter(!is.na(family)) |>
  mutate(
    medium = str_extract(family, "^(Movies|Books|TV Shows|Comic Books|Comic Strips|Authors)(?=:)"),
    series = str_trim(str_remove(family, "^[^:]+:\\s*"))
  ) |>
  filter(!is.na(medium)) |>
  # one row per game × series (a game may carry multiple tags; count it once per series)
  distinct(game_id, series, medium)

# ── 3b. Manual exceptions: franchises BGG tags under Theme: rather than Books: ──
# Cthulhu Mythos is tagged "Theme: Cthulhu Mythos" but originates from Lovecraft's fiction
exceptions <- families_raw |>
  filter(!is.na(family)) |>
  mutate(
    series = case_when(
      family == "Theme: Cthulhu Mythos" ~ "Cthulhu Mythos",
      TRUE ~ NA_character_
    ),
    medium = case_when(
      family == "Theme: Cthulhu Mythos" ~ "Books",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(series)) |>
  distinct(game_id, series, medium)

pop_tagged <- bind_rows(pop_tagged, exceptions) |>
  distinct(game_id, series, medium)

# ── 4. Count games per series, keep series with ≥ 2 games ─────────────────────
# Collapse Comic Books / Comic Strips into one label for the legend
counts <- pop_tagged |>
  mutate(medium_label = case_when(
    medium %in% c("Comic Books", "Comic Strips") ~ "Comic Books / Strips",
    medium == "Authors" ~ "Books",
    TRUE ~ medium
  )) |>
  count(series, medium_label, name = "n_games") |>
  filter(
    n_games >= 2,
    # Drop generic / noisy tags that aren't real franchises
    !series %in% c("Horror Movies", "Mystery Novels", "DreamWorks Animation",
                   "Pixar", "Hanna-Barbera", "Tark Mees Taskus",
                   "Beatrix Potter", "Charles Perrault", "The Brothers Grimm")
  ) |>
  arrange(desc(n_games)) |>
  mutate(series = fct_reorder(series, n_games))

# ── 5. Colours by medium ───────────────────────────────────────────────────────
medium_colours <- c(
  "Movies"               = "#E63946",
  "TV Shows"             = "#F4A261",
  "Books"                = "#2A9D8F",
  "Comic Books / Strips" = "#457B9D"
)

# ── 6. Plot ────────────────────────────────────────────────────────────────────
ggplot(counts, aes(x = n_games, y = series, fill = medium_label)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = n_games),
    hjust = -0.25, size = 3.3, colour = "grey25", fontface = "bold"
  ) +
  scale_fill_manual(values = medium_colours, name = "Origin medium") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Board Game Empires",
    subtitle = paste0(
      "Number of titles among BGG\u2019s 1,000 most-rated games\n",
      "belonging to each pop-culture franchise (source: BGG family tags)"
    ),
    x       = "Number of games in dataset",
    y       = NULL,
    caption = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 10: Pop Culture"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 22, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10.5, lineheight = 1.3,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    axis.text.y        = element_text(size = 11, colour = "grey15"),
    axis.text.x        = element_text(size = 9, colour = "grey45"),
    axis.title.x       = element_text(size = 9, colour = "grey45", margin = margin(t = 8)),
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 8.5),
    legend.key.size    = unit(0.45, "cm"),
    plot.margin        = margin(15, 30, 15, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day10_popculture.png", width = 11, height = 9, dpi = 300)
message("Saved images/day10_popculture.png")
