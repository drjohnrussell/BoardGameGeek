library(tidyverse)
library(gganimate)
library(scales)
library(gifski)

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0)

mechanics <- readRDS("mechanics_raw_1000.rds") |>
  mutate(game_id = as.numeric(game_id))

top1000 <- ranks |>
  arrange(rank) |>
  slice_head(n = 1000) |>
  select(id, yearpublished)

game_mech_year <- top1000 |>
  inner_join(mechanics, by = c("id" = "game_id")) |>
  filter(yearpublished >= 1993, yearpublished <= 2024) |>
  mutate(mechanic = str_trunc(mechanic, 28, side = "right", ellipsis = "…"))

# ── Cumulative counts per year ─────────────────────────────────────────────────
years <- 1993:2024
n_top <- 10

cumulative <- map_dfr(years, \(yr) {
  game_mech_year |>
    filter(yearpublished <= yr) |>
    count(mechanic, name = "count") |>
    mutate(year = yr)
})

ranked <- cumulative |>
  group_by(year) |>
  mutate(rank = rank(-count, ties.method = "first")) |>
  filter(rank <= n_top) |>
  mutate(yr_max = max(count)) |>
  ungroup()

# ── Colors ────────────────────────────────────────────────────────────────────
final_order <- ranked |>
  filter(year == 2024) |>
  arrange(rank) |>
  pull(mechanic)

all_mechs_race  <- unique(ranked$mechanic)
other_mechs     <- sort(setdiff(all_mechs_race, final_order))
ordered_mechs   <- c(final_order, other_mechs)

pal <- colorRampPalette(c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#9C755F", "#BAB0AC", "#D4A6C8",
  "#6DAEDB", "#C45BAA", "#F0A500", "#2E8B57", "#CD5C5C"
))(length(ordered_mechs))

mech_colors <- setNames(pal, ordered_mechs)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(ranked,
            aes(x = -rank, y = count, fill = mechanic, group = mechanic)) +
  # Bars
  geom_col(width = 0.80, show.legend = FALSE) +
  # Mechanic name — inside bar, left-aligned at 2% of axis max
  geom_text(
    aes(y = yr_max * 0.02, label = mechanic),
    hjust = 0, size = 4.8, colour = "white", fontface = "bold",
    family = "sans"
  ) +
  # Count label — inside bar, right-aligned near bar tip
  geom_text(
    aes(y = count - yr_max * 0.01, label = comma(count)),
    hjust = 1, size = 4.2, colour = "white",
    family = "sans"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = mech_colors) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(
    labels = comma,
    breaks = function(lims) {
      b <- pretty(lims, n = 5)
      b[b == floor(b) & b >= 0]
    },
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Most Common Mechanics in BGG's Top 1,000 Games",
    subtitle = "Cumulative count through {closest_state}",
    x        = NULL,
    y        = "Cumulative games published",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 27: Animate"
  ) +
  theme_minimal(base_family = "sans", base_size = 15) +
  theme(
    plot.title    = element_text(size = 22, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 15, colour = "#555555", margin = margin(b = 10)),
    plot.caption  = element_text(size = 10, colour = "#999999", margin = margin(t = 10)),
    plot.margin   = margin(t = 16, r = 50, b = 14, l = 16),
    axis.text.y   = element_blank(),
    axis.text.x   = element_text(size = 11, colour = "grey50"),
    axis.title.x  = element_text(size = 12, colour = "grey50", margin = margin(t = 6)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#DDDDDD", linewidth = 0.5)
  ) +
  transition_states(year, transition_length = 3, state_length = 1) +
  ease_aes("cubic-in-out") +
  view_follow(fixed_x = TRUE)

# ── Render ────────────────────────────────────────────────────────────────────
animate(
  p,
  nframes  = 420,
  fps      = 12,
  width    = 1000,
  height   = 640,
  res      = 130,
  renderer = gifski_renderer("images/day27_animate.gif")
)

message("Saved images/day27_animate.gif")
