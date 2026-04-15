library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(forcats)
library(scales)

# ── 1. Load data ───────────────────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0)

phys <- readRDS("physical_raw.rds")   # game_id, avg_weight — top 500 by rank

# ── 2. Pivot sub-categories to long ───────────────────────────────────────────
subcat_cols <- c(
  abstracts_rank      = "Abstract",
  cgs_rank            = "Customizable",
  childrensgames_rank = "Children's",
  familygames_rank    = "Family",
  partygames_rank     = "Party",
  strategygames_rank  = "Strategy",
  thematic_rank       = "Thematic",
  wargames_rank       = "Wargames"
)

long <- games |>
  select(id, all_of(names(subcat_cols))) |>
  pivot_longer(
    cols      = all_of(names(subcat_cols)),
    names_to  = "subcat_col",
    values_to = "subcat_rank"
  ) |>
  filter(!is.na(subcat_rank)) |>
  mutate(category = subcat_cols[subcat_col])

# ── 3. Join weight, assign tiers ───────────────────────────────────────────────
flow_df <- long |>
  inner_join(phys |> mutate(game_id = as.integer(game_id)) |>
               select(game_id, avg_weight), by = c("id" = "game_id")) |>
  mutate(
    weight_tier = case_when(
      avg_weight <= 2.0              ~ "Light\n(\u22642.0)",
      avg_weight <= 3.0              ~ "Medium\n(2.0\u20133.0)",
      avg_weight <= 4.0              ~ "Medium-Heavy\n(3.0\u20134.0)",
      TRUE                           ~ "Heavy\n(>4.0)"
    ),
    weight_tier = factor(weight_tier, levels = c(
      "Light\n(\u22642.0)", "Medium\n(2.0\u20133.0)",
      "Medium-Heavy\n(3.0\u20134.0)", "Heavy\n(>4.0)"
    ))
  ) |>
  count(category, weight_tier, name = "n_games")

# Order categories by mean weight (lightest at top)
cat_order <- flow_df |>
  group_by(category) |>
  summarise(wt = sum(n_games * as.numeric(weight_tier)) / sum(n_games),
            .groups = "drop") |>
  arrange(wt) |>
  pull(category)

flow_df <- flow_df |>
  mutate(category = factor(category, levels = cat_order))

# ── 4. Colours by sub-category ─────────────────────────────────────────────────
cat_colours <- c(
  "Children's"  = "#4DAF4A",
  "Party"       = "#FF7F00",
  "Family"      = "#A6CEE3",
  "Abstract"    = "#1F78B4",
  "Customizable"= "#CAB2D6",
  "Thematic"    = "#E31A1C",
  "Strategy"    = "#6A3D9A",
  "Wargames"    = "#B15928"
)

# ── 5. Plot ────────────────────────────────────────────────────────────────────
ggplot(flow_df,
       aes(axis1 = category, axis2 = weight_tier, y = n_games)) +
  geom_alluvium(aes(fill = category), width = 1/4, alpha = 0.75, knot.pos = 0.4) +
  geom_stratum(width = 1/4, fill = "grey92", colour = "grey60", linewidth = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.2, colour = "grey20", fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = cat_colours, guide = "none") +
  scale_x_discrete(
    limits = c("category", "weight_tier"),
    labels = c("BGG Sub-category", "Complexity"),
    expand = expansion(add = c(0.15, 0.15))
  ) +
  labs(
    title    = "The Board Game Ecosystem",
    subtitle = paste0(
      "How the top 500 ranked games (by BGG score) flow from sub-category\n",
      "into complexity tier. A game may belong to multiple sub-categories."
    ),
    y       = "Number of games",
    caption = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 13: Ecosystems"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 20, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10, lineheight = 1.3,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid          = element_blank(),
    axis.text.x        = element_text(size = 11, colour = "grey20", face = "bold"),
    axis.text.y        = element_blank(),
    axis.title.y       = element_blank(),
    plot.margin        = margin(15, 20, 15, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day13_ecosystems.png", width = 11, height = 9, dpi = 300)
