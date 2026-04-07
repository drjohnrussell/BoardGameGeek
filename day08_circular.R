library(dplyr)
library(tidyr)
library(circlize)
library(RColorBrewer)

# ── 1. Load cached mechanics (top 500 non-expansion games from Day 1) ─────────
mech <- readRDS("mechanics_raw.rds") |>
  filter(!is.na(mechanic))

# ── 2. Select top 12 mechanics by frequency ───────────────────────────────────
top_mechs <- mech |>
  count(mechanic, sort = TRUE) |>
  slice_head(n = 12) |>
  pull(mechanic)

mech_top <- mech |> filter(mechanic %in% top_mechs)

# ── 3. Build co-occurrence matrix ─────────────────────────────────────────────
# Each row in mech_top is (game_id, mechanic). Self-join on game_id gives all
# mechanic pairs that appear together in the same game.
co_df <- mech_top |>
  inner_join(mech_top, by = "game_id", relationship = "many-to-many") |>
  filter(mechanic.x != mechanic.y) |>
  count(mechanic.x, mechanic.y, name = "n")

co_mat <- matrix(
  0L,
  nrow = length(top_mechs),
  ncol = length(top_mechs),
  dimnames = list(top_mechs, top_mechs)
)
for (i in seq_len(nrow(co_df))) {
  co_mat[co_df$mechanic.x[i], co_df$mechanic.y[i]] <- co_df$n[i]
}

# ── 4. Shorten long mechanic names for readability ────────────────────────────
short_names <- c(
  "Hand Management"              = "Hand\nMgmt",
  "Variable Player Powers"       = "Variable\nPowers",
  "Dice Rolling"                 = "Dice\nRolling",
  "Set Collection"               = "Set\nCollection",
  "Area Majority / Influence"    = "Area\nMajority",
  "Cooperative Game"             = "Cooperative",
  "Worker Placement"             = "Worker\nPlacement",
  "Deck, Bag, and Pool Building" = "Deck\nBuilding",
  "Modular Board"                = "Modular\nBoard",
  "Tile Placement"               = "Tile\nPlacement",
  "Push Your Luck"               = "Push\nYour Luck",
  "Action Points"                = "Action\nPoints",
  "Network and Route Building"   = "Network\nBuilding",
  "Card Drafting"                = "Card\nDrafting",
  "Open Drafting"                = "Open\nDrafting",
  "Solo / Solitaire Game"        = "Solo /\nSolitaire",
  "Game Bonuses"                 = "Game\nBonuses",
  "Simultaneous Action Selection" = "Simultaneous\nAction",
  "Trading"                      = "Trading",
  "Auction/Bidding"              = "Auction /\nBidding"
)

rownames(co_mat) <- ifelse(
  rownames(co_mat) %in% names(short_names),
  short_names[rownames(co_mat)],
  rownames(co_mat)
)
colnames(co_mat) <- rownames(co_mat)

# ── 5. Colours — one warm/cool palette per sector ─────────────────────────────
sector_cols <- setNames(
  colorRampPalette(
    c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2",
      "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7",
      "#9C755F", "#BAB0AC", "#D4A6C8", "#86BCB6")
  )(nrow(co_mat)),
  rownames(co_mat)
)

# ── 6. Draw and save ──────────────────────────────────────────────────────────
png("images/day08_circular.png", width = 3200, height = 3200, res = 300)

par(
  mar    = c(4, 2, 5, 2),
  bg     = "#F7F5F0",
  family = "sans"
)

circos.par(
  start.degree  = 90,
  gap.after     = 4,
  track.margin  = c(0.01, 0.01),
  cell.padding  = c(0.02, 0, 0.02, 0)
)

chordDiagram(
  co_mat,
  grid.col         = sector_cols,
  transparency     = 0.35,
  annotationTrack  = "grid",
  preAllocateTracks = list(track.height = 0.12),
  directional      = 0,
  symmetric        = TRUE
)

# Curved sector labels
circos.track(
  track.index = 1,
  panel.fun   = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing     = "clockwise",
      niceFacing = TRUE,
      adj        = c(0, 0.5),
      cex        = 0.72,
      col        = "grey15",
      font       = 2
    )
  },
  bg.border = NA
)

# Title and annotations — use mtext so they sit outside circos coordinate system
title(
  main     = "Which mechanics go hand-in-hand in BGG's top 500 games?",
  cex.main = 1.45,
  font.main = 2,
  col.main = "#1A1A1A",
  line     = 2.5
)
mtext(
  "Arc width is proportional to the number of top-500 games that share both mechanics.\nThicker arcs reveal the core pairings of the modern board game era.",
  side  = 3,
  line  = 0.4,
  cex   = 0.78,
  col   = "grey35"
)
mtext(
  "Source: BoardGameGeek API  \u2022  30 Day Chart Challenge \u2013 Day 8: Circular",
  side  = 1,
  line  = 2.5,
  cex   = 0.65,
  col   = "grey55"
)

circos.clear()
dev.off()

message("Saved \u2192 images/day08_circular.png")
