library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

# ── 1. Load data ──────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(rank), rank > 0)

# ── 2. Pivot to long: one row per (game, category) ───────────────────────────
cat_cols <- c(
  "abstracts_rank", "cgs_rank", "childrensgames_rank",
  "familygames_rank", "partygames_rank", "strategygames_rank",
  "thematic_rank", "wargames_rank"
)

cat_labels <- c(
  "Abstract", "Customizable", "Children's",
  "Family", "Party", "Strategy",
  "Thematic", "Wargames"
)

ranked_long <- games |>
  select(id, name, rank, all_of(cat_cols)) |>
  pivot_longer(
    cols      = all_of(cat_cols),
    names_to  = "category_col",
    values_to = "cat_rank"
  ) |>
  filter(!is.na(cat_rank)) |>
  mutate(
    category = factor(
      cat_labels[match(category_col, cat_cols)],
      levels = cat_labels
    ),
    name = sub("\\s*\\(.*", "", name)
  )

# ── 3. Select labels ──────────────────────────────────────────────────────────
# Niche champions: top in category (cat_rank ≤ 5) but not top overall (rank > 500)
niche <- ranked_long |>
  filter(cat_rank <= 5, rank > 500) |>
  arrange(rank) |>
  slice_head(n = 10)

# Elite: top overall AND top in their best category
elite <- ranked_long |>
  filter(rank <= 15) |>
  group_by(name) |>
  slice_min(cat_rank, n = 1, with_ties = FALSE) |>
  ungroup()

# Crossover hits: top 20 overall whose best category rank is > 30
crossover <- ranked_long |>
  filter(rank <= 20) |>
  group_by(name) |>
  mutate(best_cat_rank = min(cat_rank)) |>
  ungroup() |>
  filter(best_cat_rank > 30) |>
  group_by(name) |>
  slice_min(cat_rank, n = 1, with_ties = FALSE) |>
  ungroup()

label_pts <- bind_rows(niche, elite, crossover) |>
  distinct(name, .keep_all = TRUE)

# ── 4. Axis break positions (actual values, log scale handles transform) ──────
x_breaks <- c(1, 5, 25, 100, 500, 2500)
y_breaks <- c(1, 10, 100, 1000, 10000, 100000)

# ── 5. Plot ───────────────────────────────────────────────────────────────────
ggplot(ranked_long, aes(x = cat_rank, y = rank, colour = category)) +

  # Reference diagonal: cat rank = overall rank (same scale)
  geom_abline(
    slope     = 1, intercept = 0,
    colour    = "grey60", linetype = "dashed", linewidth = 0.55
  ) +

  # Soft threshold lines at 100
  geom_vline(xintercept = 100, colour = "grey82", linetype = "dotted", linewidth = 0.4) +
  geom_hline(yintercept = 100, colour = "grey82", linetype = "dotted", linewidth = 0.4) +

  # All (game, category) pairs
  geom_point(alpha = 0.30, size = 0.9) +

  # Labelled games
  geom_point(
    data      = label_pts,
    size      = 2.2, alpha = 0.9
  ) +

  geom_text_repel(
    data           = label_pts,
    aes(label      = name),
    size           = 2.4,
    fontface       = "bold",
    box.padding    = 0.45,
    point.padding  = 0.3,
    max.overlaps   = 25,
    segment.colour = "grey55",
    segment.size   = 0.3,
    show.legend    = FALSE
  ) +

  # Quadrant labels
  annotate(
    "text", x = 1.2, y = 8000,
    label    = "Top in their category,\nforgotten on the global list",
    colour   = "grey45", size = 2.7, hjust = 0, fontface = "italic", lineheight = 1.2
  ) +
  annotate(
    "text", x = 10, y = 3,
    label    = "Global elite, but merely\ncompetent within a category",
    colour   = "grey45", size = 2.7, hjust = 0, fontface = "italic", lineheight = 1.2
  ) +

  scale_x_log10(
    name   = "Category rank  (log scale \u2014 max ~5,000)",
    breaks = x_breaks,
    labels = comma
  ) +
  scale_y_log10(
    name   = "Overall BGG rank  (log scale \u2014 max ~100,000)",
    breaks = y_breaks,
    labels = comma
  ) +

  scale_colour_brewer(palette = "Dark2", name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_cartesian(clip = "off") +

  labs(
    title    = "Niche champions vs. crossover hits: two scales of BGG rank",
    subtitle = paste0(
      "Each point is a (game, category) pair \u2014 games appear once per BGG sub-category they are ranked in.\n",
      "The dashed diagonal marks where category rank = overall rank.\n"
    ),
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 7: Multiscale"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(colour = "grey40", size = 9.5, lineheight = 1.4,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(colour = "grey60", size = 8, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.text      = element_text(size = 10),
    plot.margin      = margin(15, 20, 15, 15),
    plot.background  = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day07_multiscale.png", width = 13, height = 10, dpi = 300)
