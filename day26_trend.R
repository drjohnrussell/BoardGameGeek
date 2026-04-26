library(tidyverse)
library(scales)
library(showtext)
library(ggrepel)

font_add_google("Libre Baskerville", "baskerville")
font_add_google("Source Sans 3", "sourcesans")
showtext_auto()

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0)

mechanics <- readRDS("mechanics_raw_1000.rds") |>
  mutate(game_id = as.numeric(game_id))

# Top-1000 games with year published
top1000 <- ranks |>
  arrange(rank) |>
  slice_head(n = 1000) |>
  select(id, name, yearpublished, rank, bayesaverage)

# Join and flag solo games; restrict to 1985–2024
game_solo <- top1000 |>
  left_join(
    mechanics |>
      filter(mechanic == "Solo / Solitaire Game") |>
      distinct(game_id) |>
      mutate(is_solo = TRUE),
    by = c("id" = "game_id")
  ) |>
  mutate(is_solo = replace_na(is_solo, FALSE)) |>
  filter(yearpublished >= 1985, yearpublished <= 2024)

# Annual share of solo-capable games
annual <- game_solo |>
  group_by(yearpublished) |>
  summarise(
    n_total = n(),
    n_solo  = sum(is_solo),
    pct_solo = n_solo / n_total,
    .groups = "drop"
  ) |>
  filter(n_total >= 3)

# Smoothed trend via loess
smooth_fit <- loess(pct_solo ~ yearpublished, data = annual, span = 0.35)
smooth_df <- tibble(
  yearpublished = seq(min(annual$yearpublished), max(annual$yearpublished), by = 0.5)
) |>
  mutate(
    fit = predict(smooth_fit, newdata = yearpublished),
    fit = pmax(fit, 0)
  )

# Peak annotation year
peak_year <- annual |> slice_max(pct_solo, n = 1)

# Notable annotation games (top-ranked solo games in the set)
anno_games <- game_solo |>
  filter(is_solo, yearpublished >= 2000) |>
  arrange(rank) |>
  slice_head(n = 4) |>
  left_join(annual |> select(yearpublished, pct_solo), by = "yearpublished")

# ── Palette ───────────────────────────────────────────────────────────────────
clr_line   <- "#2166AC"
clr_ribbon <- "#2166AC22"
clr_dot    <- "#2166AC"
clr_anno   <- "#D6604D"

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Ribbon under smoothed line
  geom_ribbon(
    data = smooth_df,
    aes(x = yearpublished, ymin = 0, ymax = fit),
    fill = clr_ribbon
  ) +
  # Raw annual points
  geom_point(
    data = annual,
    aes(x = yearpublished, y = pct_solo, size = n_total),
    colour = clr_dot, alpha = 0.55, shape = 16
  ) +
  # Smoothed trend line
  geom_line(
    data = smooth_df,
    aes(x = yearpublished, y = fit),
    colour = clr_line, linewidth = 1.1
  ) +
  # COVID annotation band
  annotate(
    "rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf,
    fill = "#FEE08B", alpha = 0.25
  ) +
  annotate(
    "text", x = 2021, y = 0.56, label = "COVID-19\npandemic",
    family = "sourcesans", size = 16, colour = "#B07D00",
    hjust = 0.5, lineheight = 0.4
  ) +
  # Kickstarter era annotation
  annotate(
    "segment", x = 2012, xend = 2012, y = 0, yend = 0.6,
    colour = "#888888", linewidth = 0.5, linetype = "dashed"
  ) +
  annotate(
    "text", x = 2012, y = 0.55, label = "Kickstarter era\nbegins",
    family = "sourcesans", size = 16, colour = "#555555",
    hjust = 0.5, lineheight = 0.4
  ) +
  # Scale y
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.60),
    breaks = seq(0, 0.60, 0.10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = seq(1985, 2025, 5),
    minor_breaks = seq(1985, 2025, 1),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_size_continuous(
    name   = "Games\npublished\nthat year",
    range  = c(2, 8),
    breaks = c(5, 20, 50, 100)
  ) +
  labs(
    title    = "The Rise of Solo-Capable Board Games",
    subtitle = "Share of BGG's top-1,000 ranked games with a Solo / Solitaire mechanic, by publication year.\nPoints sized by the number of top-1,000 games published that year.",
    x        = "Year published",
    y        = "Share with solo mechanic",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 26: Trend"
  ) +
  theme_minimal(base_family = "sourcesans", base_size = 20) +
  theme(
    plot.title         = element_text(family = "baskerville", size = 45,
                                      face = "bold", margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 30, colour = "#444444",
                                      lineheight = 0.5, margin = margin(b = 14)),
    plot.caption       = element_text(size = 15, colour = "#888888",
                                      margin = margin(t = 12)),
    plot.margin        = margin(18, 20, 12, 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#EEEEEE"),
    axis.text       = element_text(size = 20, colour = "#444444"),
    axis.title         = element_text(size = 25, colour = "#444444"),
    legend.position    = "right",
    legend.title       = element_text(size = 25, lineheight = .4),
    legend.text        = element_text(size = 20),
    legend.key.size    = unit(0.8, "lines")
  )

ggsave("images/day26_trend.png", p, width = 12, height = 7, dpi = 300)
