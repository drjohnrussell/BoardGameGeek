library(dplyr)
library(ggplot2)
library(scales)

# ── 1. Load data ───────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 500)

mech <- readRDS("mechanics_raw.rds") |>
  filter(!is.na(mechanic))

# ── 2. Join mechanics to ratings ───────────────────────────────────────────────
mech_ratings <- mech |>
  mutate(game_id = as.integer(game_id)) |>
  inner_join(games |> select(id, bayesaverage), by = c("game_id" = "id"))

# ── 3. Overall mean (top 500) ──────────────────────────────────────────────────
overall_mean <- mean(games$bayesaverage)
message(sprintf("Overall mean bayesaverage: %.4f", overall_mean))

# ── 4. Per-mechanic statistics ─────────────────────────────────────────────────
mech_stats <- mech_ratings |>
  group_by(mechanic) |>
  summarise(
    n      = n(),
    mean_r = mean(bayesaverage),
    sd_r   = sd(bayesaverage),
    .groups = "drop"
  ) |>
  filter(n >= 30) |>
  mutate(
    se      = sd_r / sqrt(n),
    ci_lo   = mean_r - qt(0.975, df = n - 1) * se,
    ci_hi   = mean_r + qt(0.975, df = n - 1) * se,
    diff    = mean_r - overall_mean,
    diff_lo = ci_lo  - overall_mean,
    diff_hi = ci_hi  - overall_mean
  ) |>
  arrange(diff) |>
  mutate(
    mechanic  = factor(mechanic, levels = mechanic),
    direction = if_else(diff > 0, "above", "below")
  )

message(sprintf("Mechanics shown: %d", nrow(mech_stats)))

# ── 5. Colours ─────────────────────────────────────────────────────────────────
col_above <- "#ba051a"
col_below <- "#03448e"
col_ref   <- "grey55"
col_zero  <- "grey75"

# ── 6. Plot ────────────────────────────────────────────────────────────────────
x_lo <- min(mech_stats$diff_lo) - 0.02
x_hi <- max(mech_stats$diff_hi) + 0.18   # room for n= labels

ggplot(mech_stats, aes(x = diff, y = mechanic, colour = direction)) +

  # Reference line at zero
  geom_vline(xintercept = 0, colour = col_ref, linewidth = 0.7) +

  # CI bars
  geom_errorbar(
    aes(xmin = diff_lo, xmax = diff_hi),
    width     = 0.35,
    linewidth = 0.55,
    orientation = "y"
  ) +

  # Point — size encodes number of games
  geom_point(aes(size = n)) +

  # n= labels just right of each CI bar
  geom_text(
    aes(x = diff_hi + 0.015, label = paste0("n = ", n)),
    hjust  = 0,
    size   = 2.7,
    colour = "grey45"
  ) +

  # Annotation: overall mean label on the vline
  annotate(
    "text",
    x = 0.01, y = nrow(mech_stats) + 0.6,
    label  = sprintf("Overall mean\n%.2f", overall_mean),
    hjust  = 0, vjust = 1,
    size   = 2.8,
    colour = col_ref,
    lineheight = 1.2
  ) +

  scale_colour_manual(
    values = c(above = col_above, below = col_below),
    guide  = "none"
  ) +
  scale_size_continuous(
    range  = c(2.5, 7),
    name   = "Games with\nthis mechanic",
    breaks = c(30, 60, 100, 150, 200)
  ) +
  scale_x_continuous(
    limits = c(x_lo, x_hi),
    labels = function(x) sprintf("%+.2f", x),
    expand = expansion(mult = 0)
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title    = "Which Mechanics Produce Better Board Games?",
    subtitle = paste0(
      "Mean Bayes-adjusted rating of top-500 BGG games, grouped by mechanic, relative to the overall mean.\n",
      "95% confidence intervals shown. Only mechanics present in \u226530 games included. ",
      "Point size proportional to number of games."
    ),
    x       = "Difference from overall mean Bayes-adjusted rating",
    y       = NULL,
    caption = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 16: Causation"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 19, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 9.5, lineheight = 1.35,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
    axis.text.y        = element_text(size = 10, colour = "grey15"),
    axis.text.x        = element_text(size = 9,  colour = "grey45"),
    axis.title.x       = element_text(size = 9,  colour = "grey45", margin = margin(t = 8)),
    legend.position    = "right",
    legend.title       = element_text(size = 9,  colour = "grey35"),
    legend.text        = element_text(size = 8.5, colour = "grey35"),
    plot.margin        = margin(15, 30, 10, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day16_causation.png", width = 12, height = 9, dpi = 300)
