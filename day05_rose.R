library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ── 1. Load data (non-expansions only) ────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0)

# ── 2. Count each tier per sub-category ───────────────────────────────────────
cat_cols <- c(
  "abstracts_rank", "cgs_rank", "childrensgames_rank",
  "familygames_rank", "partygames_rank", "strategygames_rank",
  "thematic_rank", "wargames_rank"
)

cat_labels <- c(
  "Abstract\nGames", "Customizable\nGames", "Children's\nGames",
  "Family\nGames", "Party\nGames", "Strategy\nGames",
  "Thematic\nGames", "Wargames"
)

rose_data <- mapply(function(col, label) {
  ranked <- !is.na(games[[col]])
  tibble(
    category = label,
    total    = sum(ranked),
    top500   = sum(ranked & !is.na(games$rank) & games$rank <= 500),
    top100   = sum(ranked & !is.na(games$rank) & games$rank <= 100)
  )
}, cat_cols, cat_labels, SIMPLIFY = FALSE) |>
  bind_rows() |>
  mutate(category = factor(category, levels = cat_labels))

# ── 3. Pivot to long — plot largest layer first so smaller ones sit on top ────
rose_long <- rose_data |>
  pivot_longer(c(total, top500, top100), names_to = "tier", values_to = "count") |>
  mutate(
    tier  = factor(tier, levels = c("total", "top500", "top100")),
    # sqrt scaling: area in polar coords is proportional to r², so sqrt(count)
    # makes petal *area* proportional to count — as in Nightingale's original
    radius = sqrt(count)
  )

# ── 4. Build label data (one row per category, placed at total radius) ─────────
label_data <- rose_data |>
  mutate(
    radius = sqrt(total),
    label  = paste0(cat_labels, "\n(", comma(total), ")")
  )

# ── 5. Colours — echoing Nightingale's original palette ───────────────────────
tier_colours <- c(
  total  = "#A8C5DA",   # muted blue  — "all ranked"
  top500 = "#C17B9E",   # dusty rose  — "top 500"
  top100 = "#7B3F6E"    # deep plum   — "top 100"
)

tier_labels <- c(
  total  = "All ranked games",
  top500 = "Top 500 overall",
  top100 = "Top 100 overall"
)

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(rose_long, aes(x = category, y = radius, fill = tier)) +

  geom_col(
    position = "identity",   # layers overlap, not stack
    width     = 0.95,
    colour    = "white",
    linewidth = 0.3,
    alpha     = 0.88
  ) +

  # Radial grid labels (manual, placed at fixed radii)
  annotate(
    "text",
    x = 1, y = sqrt(c(500, 1000, 2000, 5000)),
    label = comma(c(500, 1000, 2000, 5000)),
    size = 2.5, colour = "grey55", hjust = -0.1
  ) +

  coord_polar(start = -pi / 8) +    # slight rotation so labels clear the top

  scale_fill_manual(values = tier_colours, labels = tier_labels, name = NULL) +
  scale_y_continuous(expand = c(0, 0)) +

  labs(
    title    = "Which BGG categories punch above their weight?",
    subtitle = paste0(
      "Each petal represents one of BGG's eight game sub-categories.\n",
      "Petal area is proportional to the number of games — honouring Nightingale's original design.\n",
      "Layers show how many games in each category reach the top 500 and top 100 overall."
    ),
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 5: Experimental (Nightingale Rose)"
  ) +

  theme_void(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(colour = "grey40", size = 9.5, hjust = 0.5,
                                    lineheight = 1.4, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey60", size = 8, hjust = 0.5,
                                    margin = margin(t = 10)),
    axis.text.x      = element_text(size = 8, colour = "grey30", lineheight = 1.2),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9.5),
    legend.key.size  = unit(0.55, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    plot.margin      = margin(15, 15, 15, 15),
    plot.background  = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day05_rose.png", width = 10, height = 11, dpi = 300)
