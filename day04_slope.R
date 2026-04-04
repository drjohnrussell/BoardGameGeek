library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# ── 1. Load top 300 non-expansion games ───────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(rank), rank > 0) |>
  arrange(rank) |>
  slice_head(n = 300) |>
  mutate(penalty = average - bayesaverage)

# ── 2. Reshape to long for slope chart ────────────────────────────────────────
slope_data <- games |>
  select(name, rank, average, bayesaverage, usersrated, penalty) |>
  pivot_longer(
    cols      = c(average, bayesaverage),
    names_to  = "metric",
    values_to = "value"
  ) |>
  mutate(x = if_else(metric == "average", 1L, 2L))

# ── 3. Select games to label ──────────────────────────────────────────────────
# Most penalized by Bayes (biggest drop)
top_drops <- games |> slice_max(penalty, n = 6)
# Most-rated (barely moves — shows the other end of the story)
most_rated <- games |> slice_max(usersrated, n = 2)
# Top overall (rank 1-2) for orientation
top_overall <- games |> filter(rank <= 2)

label_games <- bind_rows(top_drops, most_rated, top_overall) |>
  distinct(name, .keep_all = TRUE)

# ── 4. Build label data for left / right sides ───────────────────────────────
label_left <- slope_data |>
  filter(name %in% label_games$name, metric == "average")

label_right <- slope_data |>
  filter(name %in% label_games$name, metric == "bayesaverage") |>
  mutate(drop_label = paste0("\u2212", sprintf("%.2f", penalty)))

# ── 5. Plot ───────────────────────────────────────────────────────────────────
ggplot(slope_data, aes(x = x, y = value, group = name)) +

  # All background lines
  geom_line(
    data      = filter(slope_data, !name %in% label_games$name),
    aes(color = log10(usersrated)),
    alpha     = 0.35,
    linewidth = 0.45
  ) +

  # Highlighted lines
  geom_line(
    data      = filter(slope_data, name %in% label_games$name),
    aes(color = log10(usersrated)),
    linewidth = 1.3,
    alpha     = 0.95
  ) +

  # Highlighted points
  geom_point(
    data  = filter(slope_data, name %in% label_games$name),
    aes(color = log10(usersrated)),
    size  = 2.5
  ) +

  # Left-side labels (raw average)
  geom_text(
    data  = label_left,
    aes(label = paste0("#", rank, " ", str_wrap(name, 20), "  "),
        color = log10(usersrated)),
    hjust      = 1,
    size       = 2,
    fontface   = "bold",
    lineheight = 0.85
  ) +

  # Right-side labels with drop amount
  geom_text(
    data  = label_right,
    aes(label = paste0("  ", name, "\n  (", drop_label, ")"),
        color = log10(usersrated)),
    hjust      = 0,
    size       = 2,
    fontface   = "bold",
    lineheight = 0.85
  ) +

  # Column header annotations — placed just inside the top of the panel
  annotate("text", x = 1, y = -Inf, label = "Raw Average",
           vjust = 1.5, fontface = "bold", size = 4.5, colour = "grey20") +
  annotate("text", x = 2, y = -Inf, label = "Bayes Average",
           vjust = 1.5, fontface = "bold", size = 4.5, colour = "grey20") +

  scale_color_viridis_c(
    option = "turbo",
    name   = "Number of ratings",
    labels = \(x) comma(round(10^x))
  ) +
  scale_x_continuous(
    limits = c(.8, 2.2),
    expand = c(0, 0),
    breaks = NULL
  ) +
  scale_y_continuous(
    breaks = seq(7.5, 9.0, 0.25),
    labels = number_format(accuracy = 0.01)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "BGG's Bayes correction: who gets penalized the most?",
    subtitle = paste0(
      "Each line connects a game's raw average rating (left) to its Bayes-adjusted rating (right).\n",
      "The Bayes formula pulls all ratings toward the site mean (~5.5) — games with fewer ratings fall furthest.\n",
      "Parenthetical values show the size of the penalty. Top 300 non-expansion games shown."
    ),
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 4: Slope",
    x        = NULL,
    y        = "Rating"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    plot.caption       = element_text(colour = "grey60", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "bottom",
    legend.key.width   = unit(2, "cm"),
    plot.margin        = margin(30, 110, 15, 110)
  )

ggsave("images/day04_slope.png", width = 13, height = 9, dpi = 300)
