library(dplyr)
library(ggplot2)
library(scales)

# ── 1. Load & sort ascending (poorest → richest) ──────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(usersrated), usersrated > 0) |>
  arrange(usersrated)

n     <- nrow(games)
total <- sum(games$usersrated)

# ── 2. Build Lorenz curve ─────────────────────────────────────────────────────
lorenz <- games |>
  mutate(
    cum_games = row_number() / n,
    cum_rated = cumsum(usersrated) / total
  ) |>
  select(cum_games, cum_rated)

lorenz <- bind_rows(tibble(cum_games = 0, cum_rated = 0), lorenz)

# ── 3. Gini coefficient (trapezoid rule) ──────────────────────────────────────
area_under <- sum(
  diff(lorenz$cum_games) *
  (head(lorenz$cum_rated, -1) + tail(lorenz$cum_rated, -1)) / 2
)
gini <- round(1 - 2 * area_under, 3)

# ── 4. Key annotation points ──────────────────────────────────────────────────
p50 <- lorenz |> filter(cum_games >= 0.50) |> slice_head(n = 1)
p90 <- lorenz |> filter(cum_games >= 0.90) |> slice_head(n = 1)
p99 <- lorenz |> filter(cum_games >= 0.99) |> slice_head(n = 1)

# ── 5. Plot ───────────────────────────────────────────────────────────────────
pal_blue <- "#2C6FAC"
pal_fill <- "#4E9AC7"

ggplot(lorenz, aes(x = cum_games, y = cum_rated)) +

  # Shaded inequality area (between diagonal and curve)
  geom_ribbon(
    aes(ymin = cum_rated, ymax = cum_games),
    fill  = pal_fill,
    alpha = 0.18
  ) +

  # Equality diagonal
  geom_abline(
    slope     = 1, intercept = 0,
    colour    = "grey55", linetype = "dashed", linewidth = 0.65
  ) +
  annotate(
    "text", x = 0.27, y = 0.33,
    label    = "Line of perfect equality",
    colour   = "grey50", size = 3, angle = 42, fontface = "italic"
  ) +

  # Lorenz curve
  geom_line(colour = pal_blue, linewidth = 1.1) +

  # ── Annotation: bottom 50% ─────────────────────────────────────────────────
  annotate(
    "segment",
    x = 0.50, xend = 0.50, y = 0, yend = p50$cum_rated,
    colour = "grey60", linetype = "dotted", linewidth = 0.5
  ) +
  annotate(
    "segment",
    x = 0, xend = 0.50, y = p50$cum_rated, yend = p50$cum_rated,
    colour = "grey60", linetype = "dotted", linewidth = 0.5
  ) +
  annotate(
    "label",
    x = 0.51, y = p50$cum_rated + 0.005,
    label    = sprintf("Half of all games share\nonly %.1f%% of ratings", p50$cum_rated * 100),
    hjust    = 0, vjust = 0, size = 2.9,
    colour   = "grey25", fill = "white", label.size = 0, lineheight = 1.2
  ) +

  # ── Annotation: top 10% ───────────────────────────────────────────────────
  annotate(
    "segment",
    x = 0.90, xend = 0.90, y = 0, yend = p90$cum_rated,
    colour = "grey60", linetype = "dotted", linewidth = 0.5
  ) +
  annotate(
    "segment",
    x = 0, xend = 0.90, y = p90$cum_rated, yend = p90$cum_rated,
    colour = "grey60", linetype = "dotted", linewidth = 0.5
  ) +
  annotate(
    "label",
    x = 0.89, y = p90$cum_rated + 0.005,
    label    = sprintf("Top 10%% of games\nclaim %.0f%% of all ratings", (1 - p90$cum_rated) * 100),
    hjust    = 1, vjust = 0, size = 2.9,
    colour   = "grey25", fill = "white", label.size = 0, lineheight = 1.2
  ) +

  # ── Annotation: top 1% ────────────────────────────────────────────────────
  annotate(
    "point",
    x = p99$cum_games, y = p99$cum_rated,
    colour = pal_blue, size = 2.5
  ) +
  annotate(
    "label",
    x = p99$cum_games - 0.01, y = p99$cum_rated - 0.03,
    label    = sprintf("Top 1%%\nholds %.0f%% of\nall ratings", (1 - p99$cum_rated) * 100),
    hjust    = 1, vjust = 1, size = 2.9,
    colour   = "grey25", fill = "white", label.size = 0, lineheight = 1.2
  ) +

  # ── Gini box ──────────────────────────────────────────────────────────────
  annotate(
    "label",
    x = 0.03, y = 0.97,
    label    = paste0("Gini coefficient\n", gini),
    hjust    = 0, vjust = 1, size = 4,
    colour   = pal_blue, fill = "white", label.size = 0.4,
    label.r  = unit(0.15, "lines"),
    fontface = "bold", lineheight = 1.2
  ) +

  # ── Scales & labels ───────────────────────────────────────────────────────
  scale_x_continuous(
    name   = "Cumulative share of games  (sorted by rating count, fewest first)",
    labels = label_percent(accuracy = 1),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name   = "Cumulative share of total user ratings",
    labels = label_percent(accuracy = 1),
    expand = c(0.01, 0)
  ) +

  labs(
    title    = "'Wealth inequality' on BoardGameGeek",
    subtitle = paste0(
      "Each game's share of total user ratings — across ", comma(n), " non-expansion games.\n",
      "The further the curve bows from the diagonal, the more unequal the distribution."
    ),
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 9: Wealth"
  ) +

  coord_fixed(ratio = 1, clip = "off") +

  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 17, colour = "grey10"),
    plot.subtitle    = element_text(colour = "grey40", size = 10, lineheight = 1.4,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.4),
    axis.title       = element_text(size = 10, colour = "grey30"),
    plot.margin      = margin(15, 20, 15, 15),
    plot.background  = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day09_wealth.png", width = 10, height = 10, dpi = 300)