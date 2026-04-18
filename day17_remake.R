library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()

# ── 1. Load & filter: top 500 ranked non-expansion games ──────────────────────
games_raw <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0, rank <= 500,
         yearpublished >= 1980, yearpublished <= 2024)

# ── 2. Assign primary sub-category ────────────────────────────────────────────
subcat_cols <- c("abstracts_rank", "cgs_rank", "childrensgames_rank",
                 "familygames_rank", "partygames_rank", "strategygames_rank",
                 "thematic_rank", "wargames_rank")

cat_map <- c(
  abstracts_rank      = "Abstract",
  cgs_rank            = "Customizable",
  childrensgames_rank = "Children's",
  familygames_rank    = "Family",
  partygames_rank     = "Party",
  strategygames_rank  = "Strategy",
  thematic_rank       = "Thematic",
  wargames_rank       = "Wargames"
)

games_subcat <- games_raw |>
  select(id, all_of(subcat_cols)) |>
  pivot_longer(-id, names_to = "cat_col", values_to = "cat_rank") |>
  filter(!is.na(cat_rank)) |>
  group_by(id) |>
  slice_min(cat_rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(primary_category = cat_map[cat_col])

games <- games_raw |>
  left_join(games_subcat |> select(id, primary_category), by = "id") |>
  mutate(primary_category = replace_na(primary_category, "Unclassified")) |>
  arrange(usersrated)   # draw largest bubbles last so they sit on top

# ── 3. Labels: top 25 by overall rank + all pre-1990 games ───────────────────
label_games <- games |> filter(rank <= 25 | yearpublished < 1990)

# ── 4. Gapminder-style palette (light background version) ─────────────────────
cat_order  <- c("Strategy", "Thematic", "Family", "Wargames",
                "Abstract", "Party", "Customizable")

cat_colors <- c(
  "Strategy"     = "#d73027",   # Gapminder red   (Asia analog)
  "Thematic"     = "#4575b4",   # Gapminder blue  (Africa analog)
  "Family"       = "#1a9850",   # Gapminder green (Americas analog)
  "Wargames"     = "#f46d43",   # orange
  "Abstract"     = "#fee090",   # pale gold/yellow (Europe analog)
  "Party"        = "#9970ab",   # purple
  "Customizable" = "#00aabb"   # teal
)

games <- games |>
  mutate(primary_category = factor(primary_category, levels = cat_order))

# ── 5. Decade divider positions & era labels ──────────────────────────────────
decade_breaks <- c(1990, 2000, 2010, 2020)

era_labels <- tibble(
  x     = c(1985, 1995, 2005, 2015, 2022.5),
  label = c("1980s", "1990s", "2000s", "2010s", "2020s")
)

# y position for era labels — just above the panel (clip = "off")
era_y <- 8.55

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(games, aes(x = yearpublished, y = bayesaverage,
                  size = usersrated, fill = primary_category)) +

  # Era dividers (drawn before bubbles)
  geom_vline(xintercept = decade_breaks,
             linetype = "dashed", colour = "grey70", linewidth = 0.45) +

  # Large watermark axis labels inside plot
  annotate("text", x = 2002, y = 7.1,
           label = "YEAR PUBLISHED",
           size = 28, colour = "grey88", fontface = "bold",
           family = "lato", hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 1982.5, y = 7.55,
           label = "RATING",
           size = 28, colour = "grey88", fontface = "bold",
           family = "lato", angle = 90, hjust = 0.5, vjust = 0.5) +

  # Bubbles — shape 21: filled with colour + darker stroke
  geom_point(shape = 21, colour = "white", stroke = 0.35, alpha = 0.82) +

  # Game labels for top 25
  geom_text_repel(
    data        = label_games,
    aes(x = yearpublished, y = bayesaverage, label = name),
    inherit.aes = FALSE,
    family      = "lato",
    size        = 5,
    fontface    = "bold",
    colour      = "grey15",
    bg.colour   = "white",
    bg.r        = 0.12,
    max.overlaps  = 40,
    seed          = 42,
    box.padding   = 0.5,
    point.padding = 0.3,
    min.segment.length = 0.25,
    segment.colour     = "grey55",
    segment.size       = 0.35
  ) +

  # Era labels above the panel (requires coord_cartesian clip = "off")
  annotate("text",
           x = era_labels$x, y = era_y,
           label = era_labels$label,
           family = "lato", size = 3.2, colour = "grey45",
           fontface = "bold", hjust = 0.5) +

  scale_fill_manual(values = cat_colors, name = "Sub-category", drop = FALSE) +
  scale_size_area(
    max_size = 24,
    breaks   = c(5000, 20000, 60000, 120000),
    labels   = label_comma(),
    name     = "No. of ratings"
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2024, by = 5),
    limits = c(1979, 2026),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(7.0, 8.5, by = 0.5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(ylim = c(6.9, 8.55), clip = "off") +
  labs(
    title    = "BGG Top 500",
    subtitle = paste0(
      "Each bubble is a top-500 ranked BGG game (published 1980\u20132024). ",
      "Bubble size = number of user ratings. Colour = BGG sub-category."
    ),
    x       = "YEAR PUBLISHED",
    y       = "RATING",
    caption = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 17: Remake (after Gapminder)"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    plot.title    = element_text(face = "bold", size = 40, colour = "grey15",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 20, colour = "grey45", lineheight = 1.3,
                                 margin = margin(b = 12)),
    plot.caption  = element_text(colour = "grey55", size = 12, hjust = 1,
                                 margin = margin(t = 8)),

    axis.text  = element_text(colour = "grey45", size = 12),
    axis.title.x = element_text(colour = "grey30", size = 16, face = "bold",
                                 margin = margin(t = 8)),
    axis.title.y = element_text(colour = "grey30", size = 16, face = "bold",
                                 margin = margin(r = 8)),

    legend.background = element_rect(fill = "white", colour = "grey85", linewidth = 0.3),
    legend.text       = element_text(colour = "grey20", size = 15),
    legend.title      = element_text(colour = "grey20", size = 20, face = "bold"),
    legend.key        = element_rect(fill = NA, colour = NA),
    legend.margin     = margin(6, 8, 6, 8),
    legend.spacing.y  = unit(2, "pt"),

    plot.margin = margin(15, 20, 10, 15)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(size = 5, alpha = 0.9, colour = "white", stroke = 0.3),
      order = 1
    ),
    size = guide_legend(
      override.aes = list(fill = "grey50", colour = "white", alpha = 0.8),
      order = 2
    )
  )

ggsave("images/day17_remake.png", width = 13, height = 9, dpi = 300)
