library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(scales)
library(patchwork)

# ── 1. Load data ───────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0)

overall_mean <- mean(games$bayesaverage)  # 5.67

# ── 2. Pivot sub-categories to long ───────────────────────────────────────────
subcat_cols <- c(
  abstracts_rank      = "Abstract Games",
  cgs_rank            = "Customizable",
  childrensgames_rank = "Children's Games",
  familygames_rank    = "Family Games",
  partygames_rank     = "Party Games",
  strategygames_rank  = "Strategy Games",
  thematic_rank       = "Thematic Games",
  wargames_rank       = "Wargames"
)

long <- games |>
  select(id, bayesaverage, all_of(names(subcat_cols))) |>
  pivot_longer(
    cols      = all_of(names(subcat_cols)),
    names_to  = "subcat_col",
    values_to = "subcat_rank"
  ) |>
  filter(!is.na(subcat_rank)) |>
  mutate(category = subcat_cols[subcat_col])

# ── 3. Order categories by median rating (lowest at bottom) ───────────────────
cat_order <- long |>
  group_by(category) |>
  summarise(med = median(bayesaverage), .groups = "drop") |>
  arrange(med) |>
  pull(category)

long <- long |>
  mutate(category = factor(category, levels = cat_order))

# ── 4. Colour scale: blue–white–red, midpoint at overall mean ─────────────────
x_limits  <- c(3.5, 9)
fill_low  <- "#03448e"
fill_mid  <- "#F7F7F7"
fill_high <- "#ba051a"

# ── 5. Main ridgeline plot ─────────────────────────────────────────────────────
p_main <- ggplot(long, aes(x = bayesaverage, y = category, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale          = 2.2,
    rel_min_height = 0.005,
    color          = "grey25",
    linewidth      = 0.4,
    gradient_lwd   = 0.5
  ) +
  # Overall mean dotted line
  geom_vline(
    xintercept = overall_mean,
    linetype   = "dotted",
    colour     = "grey35",
    linewidth  = 0.6
  ) +
  annotate(
    "text",
    x = overall_mean + 0.07, y = 8.85,
    label  = sprintf("Overall mean\n%.2f", overall_mean),
    hjust  = 0, vjust = 1,
    size   = 2.8,
    colour = "grey35",
    lineheight = 1.2
  ) +
  scale_fill_gradient2(
    low      = fill_low,
    mid      = fill_mid,
    high     = fill_high,
    midpoint = overall_mean,
    limits   = c(4, 9),
    guide    = "none"
  ) +
  scale_x_continuous(
    limits = x_limits,
    breaks = 4:9,
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.4, 1.8))) +
  labs(
    title    = "How Highly-Rated Are Board Game Categories?",
    subtitle = paste0(
      "Distribution of Bayes-adjusted ratings for all ranked non-expansion games,\n",
      "grouped by BGG sub-category. Dotted line = overall mean."
    ),
    x       = "Bayes-adjusted rating",
    y       = NULL,
    caption = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 12: FlowingData"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 20, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10, lineheight = 1.3,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
    axis.text.y        = element_text(size = 11, colour = "grey15"),
    axis.text.x        = element_text(size = 9, colour = "grey45"),
    axis.title.x       = element_text(size = 9, colour = "grey45", margin = margin(t = 8)),
    legend.position    = "none",
    plot.margin        = margin(15, 20, 4, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

p_main

# ── 6. FlowingData-style legend panel ─────────────────────────────────────────
# Small example ridge with gradient fill + colored arrow labels (BBC / FlowingData style)

# Build a mini density shape: thin vertical tiles, heights = normal density
ex_x    <- seq(4, 9, length.out = 500)
ex_dens <- dnorm(ex_x, mean = overall_mean, sd = 0.55)
ex_dens <- ex_dens / max(ex_dens)   # normalise to 0–1 height
ex_df   <- data.frame(x = ex_x, ymax = ex_dens)
tile_w  <- diff(ex_x)[1]

p_legend <- ggplot(ex_df) +
  # "How to read the chart" header
  annotate("text", x = 4, y = 1.35,
           label = "How to read the chart",
           hjust = 0, vjust = 1, size = 3, colour = "grey25", fontface = "bold") +
  # Mini ridge shape built from stacked thin tiles
  geom_rect(aes(xmin = x - tile_w / 2, xmax = x + tile_w / 2,
                ymin = 0, ymax = ymax, fill = x)) +
  # Arrow labels
  # Arrow + label: left (blue)
  annotate("segment",
           x = overall_mean - 0.25, xend = overall_mean-1,
           y = -0.12, yend = -0.12,
           colour = fill_low, linewidth = 0.6,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text", x = overall_mean - 0.32, y = -0.45,
           label = "Lower rated",
           hjust = 1, vjust = 0.5, size = 2.8, colour = fill_low, fontface = "bold") +
  # Arrow + label: right (red)
  annotate("segment",
           x = overall_mean + 0.25, xend = overall_mean + 1,
           y = -0.12, yend = -0.12,
           colour = fill_high, linewidth = 0.6,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text", x = overall_mean + 0.32, y = -0.45,
           label = "Higher rated",
           hjust = 0, vjust = 0.5, size = 2.8, colour = fill_high, fontface = "bold") +
  scale_fill_gradient2(
    low      = fill_low,
    mid      = fill_mid,
    high     = fill_high,
    midpoint = overall_mean,
    limits   = c(4, 9),
    guide    = "none"
  ) +
  scale_x_continuous(limits = x_limits, expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(limits = c(-0.6, 1.4)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey98", colour = NA),
        plot.margin     = margin(2, 20, 10, 15))

# ── 7. Compose and save ────────────────────────────────────────────────────────
p_main / p_legend +
  plot_layout(heights = c(11, 1))

ggsave("images/day12_flowingdata.png", width = 11, height = 8.5, dpi = 300)
