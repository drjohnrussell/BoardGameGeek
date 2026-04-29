library(tidyverse)
library(scales)
library(showtext)
library(ggrepel)

font_add_google("Libre Baskerville", "baskerville")
font_add_google("Source Sans 3",     "sourcesans")
showtext_auto()

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0, usersrated > 0,
         !is.na(average), !is.na(bayesaverage))

# ── Fit BGG's Bayesian model: bayesavg = (C·m + n·avg) / (C + n) ─────────────
# C = number of dummy votes; m = prior mean rating
fit <- nls(
  bayesaverage ~ (C * m + usersrated * average) / (C + usersrated),
  data    = ranks |> filter(usersrated >= 100),
  start   = list(C = 1500, m = 5.5),
  control = nls.control(maxiter = 200)
)
C_fit <- coef(fit)[["C"]]
m_fit <- coef(fit)[["m"]]

# ── Shrinkage curves ──────────────────────────────────────────────────────────
raw_avgs <- c(6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0)
n_seq    <- exp(seq(log(50), log(500000), length.out = 700))

curves <- expand_grid(raw_avg = raw_avgs, n = n_seq) |>
  mutate(bayesavg = (C_fit * m_fit + n * raw_avg) / (C_fit + n))

# Labels positioned at a fixed high-n anchor inside the plot
label_n <- 360000
curve_labels <- tibble(raw_avg = raw_avgs) |>
  mutate(
    n       = label_n,
    bayesavg = (C_fit * m_fit + n * raw_avg) / (C_fit + n),
    label   = paste0(raw_avg, "★")  # ★
  )

# ── Games to label (top 15 by BGG rank) ──────────────────────────────────────
top_games <- ranks |> arrange(rank) |> slice_head(n = 15)

# ── Colour palette ────────────────────────────────────────────────────────────
curve_pal <- setNames(
  c("#4575B4", "#74ADD1", "#ABD9E9", "#A6D96A", "#FDAE61", "#F46D43", "#D73027"),
  as.character(raw_avgs)
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Background: all games
  geom_point(
    data    = ranks |> filter(usersrated >= 100),
    aes(x = usersrated, y = bayesaverage),
    colour  = "grey70", alpha = 0.18, size = 0.5, shape = 16
  ) +
  # Prior mean reference
  geom_hline(yintercept = m_fit, linetype = "dashed",
             colour = "#888888", linewidth = 0.55) +
  annotate("text", x = 110, y = m_fit + 0.11,
           label = paste0("Prior mean  m = ", round(m_fit, 2)),
           hjust = 0, size = 13, family = "sourcesans", colour = "#777777") +
  # C threshold: the vote count at which real votes equal dummy votes (halfway point)
  geom_vline(xintercept = C_fit, linetype = "dotted",
             colour = "#888888", linewidth = 0.55) +
  annotate("text", x = C_fit * 1.15, y = 5.15,
           label = paste0("n = C ≈ ", round(C_fit), "\n(halfway point)"),
           hjust = 0, size = 12, family = "sourcesans",
           colour = "#777777", lineheight = 0.45) +
  # Shrinkage curves
  geom_line(
    data = curves,
    aes(x = n, y = bayesavg, colour = factor(raw_avg), group = factor(raw_avg)),
    linewidth = 0.95, alpha = 0.9
  ) +
  # Curve labels at right edge
  geom_text(
    data = curve_labels,
    aes(x = n, y = bayesavg, label = label, colour = factor(raw_avg)),
    hjust = 0, size = 13, family = "sourcesans", fontface = "bold"
  ) +
  # Top-15 game highlights (excluding Dune: Uprising)
  geom_point(
    data = top_games |> filter(!str_detect(name, "Uprising")),
    aes(x = usersrated, y = bayesaverage),
    shape = 21, size = 2.5, fill = "white", colour = "#333333", stroke = 0.8
  ) +
  # Dune: Imperium – Uprising highlighted in gold
  geom_point(
    data = top_games |> filter(str_detect(name, "Uprising")),
    aes(x = usersrated, y = bayesaverage),
    shape = 21, size = 3.5, fill = "#F4C430", colour = "#B8860B", stroke = 1.1
  ) +
  geom_text_repel(
    data    = top_games,
    aes(x = usersrated, y = bayesaverage, label = name,
        color=if_else(str_detect(name, "Uprising"), "#B8860B", "#333333")),
    size    = 11, family = "sourcesans", colour = "#333333",
    segment.colour = "#AAAAAA", segment.size = 0.35,
    max.overlaps = 20, box.padding = 0.5, point.padding = 0.3,
    seed = 42
  ) +
  geom_text_repel(
    data    = top_games |> filter(str_detect(name, "Uprising")),
    aes(x = usersrated, y = bayesaverage + 0.1, label = name),
    size    = 11, family = "sourcesans", colour = "#B8860B",
    segment.colour = "#B8860B", segment.size = 0.45,
    max.overlaps = 20, box.padding = 0.5, point.padding = 0.3,
    seed = 42
  ) +
  scale_x_log10(
    labels = label_comma(),
    breaks = c(100, 500, 1000, 5000, 10000, 50000, 100000, 300000),
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  scale_y_continuous(
    limits = c(5.0, 9.3),
    breaks = seq(5.0, 9.0, 0.5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_colour_manual(values = curve_pal, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title    = "The Pull of the Prior",
    subtitle = paste0(
      "BGG computes a Bayesian average: bayesavg = (C·m + n·avg) / (C + n). ",
      "Fitting the model to all ranked games gives\n",
      "C ≈ ", round(C_fit), " dummy votes at m ≈ ", round(m_fit, 2), ". ",
      "Each curve shows how a true raw average converges as ratings accumulate—\n",
      "games start near the prior mean and earn their real rating only with many votes."
    ),
    x       = "Number of user ratings (log scale)",
    y       = "Bayesian average rating",
    caption = "Source: BoardGameGeek  |  30 Day Chart Challenge – Day 28: Modeling"
  ) +
  theme_minimal(base_family = "sourcesans", base_size = 20) +
  theme(
    plot.title    = element_text(family = "baskerville", size = 45, face = "bold",
                                 margin = margin(b = 6)),
    plot.subtitle = element_text(size = 26, colour = "#444444",
                                 lineheight = 0.45, margin = margin(b = 14)),
    plot.caption  = element_text(size = 15, colour = "#888888", margin = margin(t = 12)),
    plot.margin   = margin(18, 55, 12, 18),
    panel.grid.major = element_line(colour = "#EEEEEE"),
    panel.grid.minor = element_blank(),
    axis.text  = element_text(size = 18, colour = "#444444"),
    axis.title = element_text(size = 23, colour = "#444444")
  )

ggsave("images/day28_modeling.png", p, width = 13, height = 9, dpi = 300)
