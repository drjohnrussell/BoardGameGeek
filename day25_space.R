library(tidyverse)
library(showtext)
library(scales)
library(ggrepel)

font_add_google("Orbitron",   "orbitron")
font_add_google("Space Mono", "spacemono")
showtext_auto()

# ── 1. Load data ──────────────────────────────────────────────────────────────
ranks   <- read_csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0)

families <- readRDS("families_raw.rds")

# ── 2. Filter for genuine space / sci-fi families ────────────────────────────
space_pat <- paste0(
  "^Space:|",                                         # BGG Space: prefix
  "Alien|Extraterrestri|Interplanetary|Interstellar|",
  "Galact|Cosmic|Star Wars|Star Realms|Space Base|",
  "Space Hulk|Battlestar|sci.fi|",
  "Creatures: Alien|Movies: Alien|",
  "Components: Map \\(Inter"
)

space_game_ids <- families |>
  filter(str_detect(family, regex(space_pat, ignore_case = TRUE))) |>
  pull(game_id) |>
  unique()

cat("Space games:", length(space_game_ids), "\n")

# ── 3. Join with ranks & compute uncertainty metrics ─────────────────────────
space_games <- ranks |>
  filter(id %in% space_game_ids,
         !is.na(yearpublished), yearpublished >= 1970, yearpublished <= 2024,
         !is.na(bayesaverage), usersrated >= 200) |>
  mutate(
    certainty = (log(usersrated) - log(min(usersrated))) /
                (log(max(usersrated)) - log(min(usersrated))),
    sz_core  = rescale(sqrt(usersrated), to = c(0.8, 7)),
    sz_mid   = sz_core * 2.5,
    sz_outer = sz_core * 5.5,
    al_core  = 0.6 + certainty * 0.4,
    al_mid   = certainty * 0.35,
    al_outer = certainty * 0.12
  )

cat("After filters:", nrow(space_games), "games\n")
cat("Year range:", min(space_games$yearpublished), "-", max(space_games$yearpublished), "\n")
cat("Rating range:", round(min(space_games$bayesaverage), 2),
    "-", round(max(space_games$bayesaverage), 2), "\n")

# ── 4. Labels — top games by usersrated ──────────────────────────────────────
labels_df <- space_games |>
  slice_max(usersrated, n = 18) |>
  mutate(name = str_replace(name, ": ", ":\n"))

# ── 5. Starfield backdrop ─────────────────────────────────────────────────────
set.seed(99)
yr_range  <- range(space_games$yearpublished) + c(-2, 2)
rat_range <- range(space_games$bayesaverage)  + c(-0.3, 0.3)

stars <- tibble(
  x     = runif(600, yr_range[1],  yr_range[2]),
  y     = runif(600, rat_range[1], rat_range[2]),
  sz    = runif(600, 0.05, 0.35),
  alpha = runif(600, 0.1, 0.55)
)

# ── 6. Plot ───────────────────────────────────────────────────────────────────
bg   <- "#04040F"
glow <- "#5588FF"

p <- ggplot(space_games, aes(x = yearpublished, y = bayesaverage)) +

  # Background stars
  geom_point(data = stars, aes(x = x, y = y, size = sz, alpha = alpha),
             colour = "white", shape = 16, inherit.aes = FALSE) +

  # Glow layers (outer → inner → core)
  geom_point(aes(size = sz_outer, alpha = al_outer), colour = glow,  shape = 16) +
  geom_point(aes(size = sz_mid,   alpha = al_mid),   colour = "#AACCFF", shape = 16) +
  geom_point(aes(size = sz_core,  alpha = al_core),  colour = "white",   shape = 16) +

  # Labels
  geom_text_repel(
    data          = labels_df,
    aes(label     = name),
    colour        = "#CCDDFF",
    family        = "spacemono",
    size          = 4.2,
    lineheight    = 0.4,
    segment.color = "#334466",
    segment.size  = 0.3,
    box.padding   = 0.5,
    max.overlaps  = Inf,
    seed          = 42
  ) +

  scale_size_identity() +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(1970, 2024, by = 5),
                     expand = expansion(add = 1)) +
  scale_y_continuous(breaks = seq(5, 9, by = 0.5),
                     expand = expansion(add = 0.15)) +

  labs(
    title    = "THE SPACE GAME GALAXY",
    subtitle = paste0(
      "Space & sci-fi board games in BGG's top 1,000 — ",
      "brighter stars are more certain (more user ratings)"
    ),
    x       = "Year published",
    y       = "BGG Bayes-adjusted rating",
    caption = "Source: BoardGameGeek  |  30 Day Chart Challenge – Day 25: Space"
  ) +

  theme_minimal(base_family = "spacemono") +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid.major = element_line(colour = "#1A1A40", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(family = "orbitron", face = "bold",
                                    size = 44, colour = "white",
                                    hjust = 0.5, margin = margin(t = 14, b = 6)),
    plot.subtitle    = element_text(size = 22, colour = "#8899BB",
                                    hjust = 0.5, margin = margin(b = 10)),
    plot.caption     = element_text(size = 18, colour = "#334466",
                                    hjust = 1,  margin = margin(t = 10, b = 8)),
    axis.text        = element_text(colour = "#556688", size = 18),
    axis.title       = element_text(colour = "#778899", size = 20),
    axis.title.x     = element_text(margin = margin(t = 8)),
    axis.title.y     = element_text(margin = margin(r = 8)),
    plot.margin      = margin(10, 20, 10, 20)
  )

ggsave("images/day25_space.png", p, width = 13, height = 10, dpi = 300)
