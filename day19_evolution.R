library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()

# ── 1. Load & filter ──────────────────────────────────────────────────────────
games_raw <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0,
         yearpublished >= 1980, yearpublished <= 2024)

# ── 2. Assign primary sub-category ────────────────────────────────────────────
subcat_cols <- c(
  "abstracts_rank", "cgs_rank", "childrensgames_rank",
  "familygames_rank", "partygames_rank", "strategygames_rank",
  "thematic_rank", "wargames_rank"
)

cat_labels <- c(
  abstracts        = "Abstract Games",
  cgs              = "Customizable Games",
  childrensgames   = "Children's Games",
  familygames      = "Family Games",
  partygames       = "Party Games",
  strategygames    = "Strategy Games",
  thematic         = "Thematic Games",
  wargames         = "Wargames"
)

# Pivot to long, keep only rows with a sub-rank, pick best sub-rank per game
primary_cat <- games_raw |>
  select(id, yearpublished, all_of(subcat_cols)) |>
  pivot_longer(all_of(subcat_cols), names_to = "cat_raw", values_to = "sub_rank") |>
  filter(!is.na(sub_rank)) |>
  group_by(id) |>
  slice_min(sub_rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(category = cat_labels[sub(pattern = "_rank$", replacement = "", cat_raw)])

all_games <- primary_cat |> select(id, yearpublished, category)

# ── 3. Proportions by year ────────────────────────────────────────────────────
year_props <- all_games |>
  count(yearpublished, category) |>
  group_by(yearpublished) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# Factor order: Strategy on top (largest), others beneath
cat_order <- c(
  "Abstract Games",
  "Children's Games",
  "Customizable Games",
  "Family Games",
  "Party Games",
  "Thematic Games",
  "Wargames",
  "Strategy Games"
)

year_props <- year_props |>
  mutate(category = factor(category, levels = cat_order))

# ── 4. Colour palette ─────────────────────────────────────────────────────────
pal <- c(
  "Abstract Games"      = "#4e79a7",
  "Children's Games"    = "#f28e2b",
  "Customizable Games"  = "#e15759",
  "Family Games"        = "#76b7b2",
  "Party Games"         = "#59a14f",
  "Thematic Games"      = "#edc948",
  "Wargames"            = "#b07aa1",
  "Strategy Games"      = "#ff9da7"
)

# ── 5. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(year_props, aes(x = yearpublished, y = prop, fill = category)) +
  geom_area(position = "fill", colour = "white", linewidth = 0.15, alpha = 0.92) +
  scale_x_continuous(
    breaks = seq(1980, 2024, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = pal, name = NULL) +
  labs(
    title    = "The Evolving Landscape of Board Games",
    subtitle = "Share of ranked BGG games by sub-category, by year published (1980–2024)",
    x        = NULL,
    y        = "Share of games published",
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge – Day 19: Evolution"
  ) +
  theme_minimal(base_family = "lato", base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 50, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 40, colour = "#555555", margin = margin(b = 12)),
    plot.caption     = element_text(size = 25, colour = "#888888", hjust = 1),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#eeeeee"),
    axis.text        = element_text(size = 40),
    legend.position  = "bottom",
    legend.text      = element_text(size = 40),
    legend.key.size  = unit(0.55, "cm"),
    plot.margin      = margin(16, 16, 12, 16)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("images/day19_evolution.png", p, width = 13, height = 7, dpi = 300)
