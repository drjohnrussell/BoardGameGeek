library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

# ── 1. Load top 1000 non-expansion games ──────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(rank), rank > 0) |>
  arrange(rank) |>
  slice_head(n = 1000)

# ── 2. Assign each game to its rarest type (same priority as Day 2) ───────────
type_meta <- tribble(
  ~type,          ~col,
  "Party",        "partygames_rank",
  "Children's",   "childrensgames_rank",
  "Card Games",   "cgs_rank",
  "War Games",    "wargames_rank",
  "Abstracts",    "abstracts_rank",
  "Family",       "familygames_rank",
  "Thematic",     "thematic_rank",
  "Strategy",     "strategygames_rank"
)

games_typed <- games |>
  mutate(
    type = map_chr(row_number(), function(i) {
      g     <- games[i, ]
      match <- type_meta$type[map_lgl(type_meta$col, \(col) !is.na(g[[col]]))]
      if (length(match) == 0) "Other" else match[[1]]
    }),
    # Collapse small / uncategorized types into "Other"
    type = case_when(
      type %in% c("Party", "Children's") ~ "Other",
      TRUE ~ type
    ),
    type = factor(type, levels = c(
      "Strategy", "Thematic", "Family", "Abstracts",
      "War Games", "Card Games", "Other"
    ))
  )

# ── 3. Assign publication decade ──────────────────────────────────────────────
games_typed <- games_typed |>
  mutate(
    decade = case_when(
      yearpublished < 2000 ~ "Pre-2000",
      yearpublished < 2010 ~ "2000s",
      yearpublished < 2020 ~ "2010s",
      TRUE                 ~ "2020s"
    ),
    decade = factor(decade, levels = c("Pre-2000", "2000s", "2010s", "2020s"))
  )

# ── 4. Define colours per type ────────────────────────────────────────────────
type_colors <- c(
  "Strategy"   = "#2471a3",
  "Thematic"   = "#7d3c98",
  "Family"     = "#1e8449",
  "Abstracts"  = "#566573",
  "War Games"  = "#922b21",
  "Card Games" = "#d68910",
  "Other"      = "#aab7b8"
)

# ── 5. Compute tile rectangles for the mosaic ─────────────────────────────────
# Row heights = proportion of games per decade
# Column widths within each row = proportion of each type within that decade

mosaic_data <- games_typed |>
  count(decade, type, .drop = FALSE) |>
  group_by(decade) |>
  mutate(
    prop_in_decade = n / sum(n),
    xmax = cumsum(prop_in_decade),
    xmin = xmax - prop_in_decade
  ) |>
  ungroup() |>
  group_by(decade) |>
  mutate(decade_n = sum(n)) |>
  ungroup()

# Compute row y positions (decades stacked on y-axis)
decade_heights <- mosaic_data |>
  distinct(decade, decade_n) |>
  arrange(decade) |>
  mutate(
    total      = sum(decade_n),
    row_height = decade_n / total,
    ymax       = cumsum(row_height),
    ymin       = ymax - row_height,
    y_mid      = (ymin + ymax) / 2
  )

mosaic_data <- mosaic_data |>
  left_join(decade_heights |> select(decade, ymin, ymax, y_mid), by = "decade")

# Midpoints for tile labels (only label tiles big enough to read)
mosaic_data <- mosaic_data |>
  mutate(
    x_mid     = (xmin + xmax) / 2,
    tile_w    = xmax - xmin,
    tile_h    = ymax - ymin,
    show_pct  = tile_w > 0.07 & tile_h > 0.04,
    pct_label = paste0(round(prop_in_decade * 100), "%")
  )

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(mosaic_data) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
    colour = "white", linewidth = 0.8
  ) +
  # Percentage labels inside tiles
  geom_text(
    data = filter(mosaic_data, show_pct),
    aes(x = x_mid, y = y_mid, label = pct_label),
    colour = "white", size = 3, fontface = "bold"
  ) +
  # Decade labels on right (with game count)
  geom_text(
    data = distinct(mosaic_data, decade, ymin, ymax, y_mid, decade_n),
    aes(x = 1.03, y = y_mid,
        label = paste0(decade, "\n(n=", decade_n, ")")),
    size = 3.2, colour = "grey30", hjust = 0
  ) +
  scale_fill_manual(values = type_colors, name = "Game type") +
  scale_x_continuous(expand = c(0, 0),
                     labels = scales::percent_format(),
                     name   = "Share within decade") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "How has the mix of board game types shifted over time?",
    subtitle = "Top 1,000 BGG-ranked non-expansion games by publication decade and type.\nEach game assigned to its rarest category; Party, Children's, and uncategorized games grouped as 'Other'.\nRow height reflects the share of the 1,000 games published in that decade.",
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge – Day 3: Mosaic",
    y        = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey60", size = 8),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    panel.grid       = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    plot.margin      = margin(20, 110, 15, 20)
  )

ggsave("images/day03_mosaic.png", width = 12, height = 8, dpi = 300)
