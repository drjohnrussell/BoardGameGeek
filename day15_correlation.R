library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ── 1. Load data ───────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0)

# Restrict to years with enough games to smooth reliably; drop extreme outliers
games_filt <- games |>
  filter(yearpublished >= 1970, yearpublished <= 2024,
         usersrated >= 50)

# ── 2. Pivot to long for dual smoothed lines ───────────────────────────────────
games_long <- games_filt |>
  select(id, yearpublished, average, bayesaverage, usersrated) |>
  pivot_longer(
    cols      = c(average, bayesaverage),
    names_to  = "rating_type",
    values_to = "rating"
  ) |>
  mutate(
    rating_label = if_else(rating_type == "average", "Raw user rating", "Bayes-adjusted rating")
  )

# ── 3. Compute smoothed means for the ribbon ──────────────────────────────────
# We'll let geom_smooth handle the lines; build a ribbon from the two smooths
# by predicting on a common year grid
year_grid <- tibble(yearpublished = seq(1970, 2024, by = 0.5))

fit_avg   <- loess(average      ~ yearpublished, data = games_filt, span = 0.3)
fit_bayes <- loess(bayesaverage ~ yearpublished, data = games_filt, span = 0.3)

ribbon_df <- year_grid |>
  mutate(
    avg_pred   = predict(fit_avg,   newdata = year_grid),
    bayes_pred = predict(fit_bayes, newdata = year_grid)
  )

# ── 4. Colour palette ─────────────────────────────────────────────────────────
col_avg   <- "#e07b39"   # warm orange — raw rating
col_bayes <- "#2c6fad"   # steel blue  — Bayes-adjusted

# ── 5. Plot ────────────────────────────────────────────────────────────────────
ggplot() +
  # Individual game points — very light, one layer per rating type
  geom_point(
    data = games_long |> filter(rating_type == "average"),
    aes(x = yearpublished, y = rating),
    colour = col_avg, alpha = 0.06, size = 0.55, shape = 16
  ) +
  geom_point(
    data = games_long |> filter(rating_type == "bayesaverage"),
    aes(x = yearpublished, y = rating),
    colour = col_bayes, alpha = 0.06, size = 0.55, shape = 16
  ) +
  # Ribbon between the two smoothed lines
  geom_ribbon(
    data = ribbon_df,
    aes(x = yearpublished, ymin = pmin(avg_pred, bayes_pred),
        ymax = pmax(avg_pred, bayes_pred)),
    fill = "grey70", alpha = 0.35
  ) +
  # Smoothed trend lines
  geom_smooth(
    data    = games_long |> filter(rating_type == "average"),
    aes(x = yearpublished, y = rating),
    method  = "loess", formula = y ~ x, span = 0.3,
    colour  = col_avg, linewidth = 1.1, se = FALSE
  ) +
  geom_smooth(
    data    = games_long |> filter(rating_type == "bayesaverage"),
    aes(x = yearpublished, y = rating),
    method  = "loess", formula = y ~ x, span = 0.3,
    colour  = col_bayes, linewidth = 1.1, se = FALSE
  ) +
  # Line labels — placed near 2024 end of each curve
  annotate("text", x = 2025, y = predict(fit_avg,   data.frame(yearpublished = 2023)) + 0.05,
           label = "Raw rating", colour = col_avg,   hjust = 0, size = 3.2, fontface = "bold") +
  annotate("text", x = 2025, y = predict(fit_bayes, data.frame(yearpublished = 2023)) - 0.15,
           label = "Bayes-adjusted", colour = col_bayes, hjust = 0, size = 3.2, fontface = "bold") +
  # Annotation: explain the gap
  annotate("text", x = 1996, y = 8.0,
           label = "The shaded gap shows where\nthe Bayes correction pulls\nscores toward the mean.\nIt widens as games accumulate\nfewer ratings.",
           hjust = 0, vjust = 1, size = 2.8, colour = "grey35", lineheight = 1.3) +
  scale_x_continuous(
    limits = c(1970, 2033),
    breaks = seq(1970, 2024, by = 10),
    expand = expansion(mult = c(0.01, 0))
  ) +
  scale_y_continuous(
    limits = c(4, 9),
    breaks = 4:9,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title    = "Cult of the New? Board Game Ratings by Year Published",
    subtitle = paste0(
      "Raw user ratings vs Bayes-adjusted ratings for all ranked non-expansion games (published 1970\u20132024,\n",
      "\u226550 ratings). Lines show loess trends; shaded band = gap between them."
    ),
    x       = "Year published",
    y       = "Rating",
    caption = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 15: Correlation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 20, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10, lineheight = 1.3,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "grey90", linewidth = 0.4),
    axis.text          = element_text(size = 10, colour = "grey40"),
    axis.title         = element_text(size = 10, colour = "grey40"),
    plot.margin        = margin(15, 80, 10, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day15_correlation.png", width = 12, height = 8, dpi = 300)
