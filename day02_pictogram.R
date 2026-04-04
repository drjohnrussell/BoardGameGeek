library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(showtext)

# Register FA Solid font bundled with waffle (changed mind and didn't end up using waffle)
font_add(
  family  = "FontAwesome5Free-Solid",
  regular = system.file("fonts/fa-solid-900.ttf", package = "waffle")
)
showtext_auto()
showtext_opts(dpi = 300)

# ── 1. Load top 100 ───────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, !is.na(rank), rank > 0) |>
  arrange(rank) |>
  slice_head(n = 100)

# ── 2. Assign each game to its rarest type ────────────────────────────────────
# Priority order: rarest type wins when a game belongs to multiple categories
type_meta <- tribble(
  ~type,         ~col,                  ~icon,    ~color,
  "Party",       "partygames_rank",     "\uf79f", "#e74c3c",
  "Card Games",  "cgs_rank",            "\uf5fd", "#d68910",
  "War Games",   "wargames_rank",       "\uf3ed", "#922b21",
  "Abstracts",   "abstracts_rank",      "\uf443", "#566573",
  "Family",      "familygames_rank",    "\uf015", "#1e8449",
  "Thematic",    "thematic_rank",       "\uf6d5", "#7d3c98",
  "Strategy",    "strategygames_rank",  "\uf441", "#2471a3"
)

# For each game, take the first (rarest) type where it has a non-NA rank
games_typed <- games |>
  mutate(
    type = map_chr(row_number(), function(i) {
      g <- games[i, ]
      match <- type_meta$type[map_lgl(type_meta$col, \(col) !is.na(g[[col]]))]
      if (length(match) == 0) "Unknown" else match[[1]]
    })
  ) |>
  left_join(type_meta |> select(type, icon, color), by = "type")

# ── 3. Compute grid positions (rank order, left-to-right, top-to-bottom) ─────
icons_per_row <- 10

plot_data <- games_typed |>
  mutate(
    pos = rank,
    x   = ((pos - 1) %% icons_per_row) + 1,
    y   = -floor((pos - 1) / icons_per_row)       # negative so row 1 is at top
  )

# ── 4. Legend data ────────────────────────────────────────────────────────────
type_counts <- games_typed |>
  count(type) |>
  left_join(type_meta |> select(type, icon, color), by = "type") |>
  arrange(desc(n)) |>
  mutate(
    legend_x = 11.8,
    legend_y = -(seq_len(n()) - 1) * 1.2
  )

# ── 5. Annotation data for rank 1, rank 100, and the party game ──────────────
annotate_games <- plot_data |>
  filter(rank %in% c(1L, 100L) | type == "Party") |>
  mutate(
    # Text label position
    label_x = case_when(
      rank == 1   ~ x,          # centred above icon
      rank == 100 ~ x - 0.3,   # upper-left of icon
      TRUE        ~ x + 0.55   # party: right of icon in open space
    ),
    label_y = case_when(
      rank == 1   ~ y + 0.52,  # above (near top edge)
      rank == 100 ~ y - 0.52,  # below (near bottom edge)
      TRUE        ~ y - 0.25           # party: same row
    ),
    # Segment start (just outside the icon)
    seg_x   = case_when(
      rank == 1   ~ x,
      rank == 100 ~ x - 0.18,
      TRUE        ~ x + 0.22
    ),
    seg_y   = case_when(
      rank == 1   ~ y + 0.18,
      rank == 100 ~ y - 0.18,
      TRUE        ~ y - 0.18
    ),
    # Segment end (just short of label)
    seg_xend = case_when(
      rank == 1   ~ label_x,
      rank == 100 ~ label_x + 0.1,
      TRUE        ~ label_x - 0.08
    ),
    seg_yend = case_when(
      rank == 1   ~ label_y - 0.1,
      rank == 100 ~ label_y + 0.1,
      TRUE        ~ label_y
    ),
    hjust = case_when(
      rank == 1   ~ 0.5,
      rank == 100 ~ 1,
      TRUE        ~ 0
    )
  )

# ── 6. Row labels (rank bands) ────────────────────────────────────────────────
row_labels <- tibble(
  y     = -(seq(0, 9)),
  label = paste0(seq(1, 100, 10), "\u2013", seq(10, 100, 10))
)

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(plot_data, aes(x = x, y = y)) +
  # Game icons
  geom_text(
    aes(label = icon, colour = color),
    family = "FontAwesome5Free-Solid",
    size   = 6
  ) +
  # Rank labels on left
  geom_text(
    data  = row_labels,
    aes(x = 0.3, y = y, label = label),
    hjust = 1, size = 3, colour = "grey50"
  ) +
  # Legend icons
  geom_text(
    data   = type_counts,
    aes(x = legend_x, y = legend_y, label = icon, colour = color),
    family = "FontAwesome5Free-Solid",
    size   = 5,
    hjust  = 0.5
  ) +
  # Legend type labels
  geom_text(
    data   = type_counts,
    aes(x = legend_x + 0.5, y = legend_y,
        label = paste0(type, " (", n, ")"), colour = color),
    hjust  = 0, size = 3.2, fontface = "bold"
  ) +
  # Annotation segments
  geom_segment(
    data   = annotate_games,
    aes(x = seg_x, y = seg_y, xend = seg_xend, yend = seg_yend),
    colour = "grey40", linewidth = 0.4
  ) +
  # Annotation labels
  geom_text(
    data     = annotate_games,
    aes(x = label_x, y = label_y, label = paste0(rank, ": ",name), hjust = hjust),
    colour   = "grey20", size = 2.8, fontface = "italic"
  ) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.5, 16.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-9.6, 0.6),  expand = c(0, 0)) +
  labs(
    title    = "BGG's Top 100 games by rank and type",
    subtitle = "Each icon = 1 game, ordered by overall rank (top-left = #1).\nIcon shape shows game type; assigned to rarest category for multi-type games.",
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge - Day 2: Pictogram"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title      = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle   = element_text(colour = "grey40", size = 9,  margin = margin(b = 10)),
    plot.caption    = element_text(colour = "grey60", size = 7.5, hjust = 1,
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 20, 15, 50)
  )

ggsave("images/day02_pictogram.png", width = 12, height = 7, dpi = 300)
