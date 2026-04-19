library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(treemapify)

# ── 1. Load data ──────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 1000) |>
  mutate(game_id = as.character(id))

families <- readRDS("families_raw.rds")

# ── 2. Filter to History: and Ancient: families ───────────────────────────────
hist_fams <- families |>
  filter(grepl("^History:|^Ancient:", family)) |>
  left_join(games |> select(game_id, name, rank, bayesaverage), by = "game_id")

# ── 3. Assign each family to a chronological era ─────────────────────────────
era_map <- c(
  "Ancient: Babylon"                            = "Ancient",
  "Ancient: Corinth"                            = "Ancient",
  "Ancient: Egypt"                              = "Ancient",
  "Ancient: Greece"                             = "Ancient",
  "Ancient: Mesopotamia"                        = "Ancient",
  "Ancient: Pompeii"                            = "Ancient",
  "Ancient: Rome"                               = "Ancient",
  "Ancient: Sparta"                             = "Ancient",
  "History: Punic Wars"                         = "Ancient",
  "History: Sicilian Wars"                      = "Ancient",
  "History: Roman Empire"                       = "Ancient",
  "History: Francia"                            = "Medieval",
  "History: Barbarian migrations and invasions" = "Medieval",
  "History: Viking Expansion"                   = "Medieval",
  "History: Age of Kings"                       = "Medieval",
  "History: Italian City-States"                = "Medieval",
  "History: Sengoku Period"                     = "Medieval",
  "History: Age of Discovery"                   = "Early Modern",
  "History: Early Imperial China"               = "Early Modern",
  "History: Ming Dynasty"                       = "Early Modern",
  "History: French and Indian War"              = "Early Modern",
  "History: Edo Period"                         = "Early Modern",
  "History: Industrial Revolution"              = "18th–19th Century",
  "History: Regency Era"                        = "18th–19th Century",
  "History: Anglo-Afghan Wars"                  = "18th–19th Century",
  "History: American Old West (Wild West)"      = "18th–19th Century",
  "History: Oregon Trail"                       = "18th–19th Century",
  "History: La Belle Epoque (1871\u20131914)"   = "18th–19th Century",
  "History: Cold War"                           = "20th Century",
  "History: Resistance during WW2 in Europe"    = "20th Century"
)

era_levels <- c("Ancient", "Medieval", "Early Modern", "18th–19th Century", "20th Century")

era_approx <- c(
  "Ancient"          = -500,
  "Medieval"         = 900,
  "Early Modern"     = 1550,
  "18th–19th Century"= 1800,
  "20th Century"     = 1950
)

hist_tagged <- hist_fams |>
  mutate(era = era_map[family]) |>
  filter(!is.na(era)) |>
  mutate(era = factor(era, levels = era_levels))

# ── 4. Count games per family, keep era for colour ───────────────────────────
# Clean up family label for display (strip the prefix)
plot_data <- hist_tagged |>
  distinct(game_id, era, family) |>
  count(era, family, name = "n_games") |>
  mutate(
    family_label = sub("^(Ancient|History): ", "", family),
    era = factor(era, levels = era_levels)
  )

# ── 5. Palette ────────────────────────────────────────────────────────────────
era_pal <- c(
  "Ancient"           = "#C0392B",
  "Medieval"          = "#8E44AD",
  "Early Modern"      = "#2471A3",
  "18th–19th Century" = "#1A8A52",
  "20th Century"      = "#D35400"
)

# ── 6. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(plot_data,
            aes(area  = n_games,
                fill  = era,
                label = paste0(family_label, "\n", n_games),
                subgroup = era)) +
  geom_treemap(color = "white", linewidth = 1.2) +
  geom_treemap_subgroup_border(color = "white", linewidth = 3) +
  geom_treemap_text(
    color     = "white",
    place     = "centre",
    grow      = FALSE,
    reflow    = TRUE,
    size      = 10,
    fontface  = "bold",
    min.size  = 6
  ) +
  geom_treemap_subgroup_text(
    color    = "white",
    alpha    = 0.25,
    place    = "topleft",
    grow     = TRUE,
    fontface = "bold"
  ) +
  scale_fill_manual(values = era_pal, name = "Era") +
  labs(
    title    = "Which eras of history inspire the most board games?",
    subtitle = "Top-1000 BGG-ranked games tagged with a historical period or Ancient family.\nTile size = number of games; colour = broad era; each family shown separately.",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 21: Historical"
  ) +
  theme(
    plot.background = element_rect(fill = "#FAFAF7", color = NA),
    plot.title      = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle   = element_text(color = "grey40", size = 10, margin = margin(b = 8)),
    plot.caption    = element_text(color = "grey60", size = 8),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9)
  )

ggsave("images/day21_historical.png", p, width = 12, height = 8, dpi = 300)
message("Saved \u2192 images/day21_historical.png")
