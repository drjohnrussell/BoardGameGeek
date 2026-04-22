library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()

# ── 1. Load & filter top 500 ──────────────────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 500)

# ── 2. Assign primary sub-category ────────────────────────────────────────────
subcat_cols <- c(
  "abstracts_rank", "cgs_rank", "childrensgames_rank",
  "familygames_rank", "partygames_rank", "strategygames_rank",
  "thematic_rank", "wargames_rank"
)

cat_labels <- c(
  abstracts       = "Abstract Games",
  cgs             = "Customizable Games",
  childrensgames  = "Children's Games",
  familygames     = "Family Games",
  partygames      = "Party Games",
  strategygames   = "Strategy Games",
  thematic        = "Thematic Games",
  wargames        = "Wargames"
)

primary_cat <- games |>
  select(id, name, rank, bayesaverage, usersrated, all_of(subcat_cols)) |>
  pivot_longer(all_of(subcat_cols), names_to = "cat_raw", values_to = "sub_rank") |>
  filter(!is.na(sub_rank)) |>
  group_by(id) |>
  slice_min(sub_rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(category = cat_labels[sub("_rank$", "", cat_raw)])

# ── 3. Order categories by median Bayes rating (descending) ───────────────────
cat_order <- primary_cat |>
  group_by(category) |>
  summarise(med = median(bayesaverage)) |>
  arrange(desc(med)) |>
  pull(category)

primary_cat <- primary_cat |>
  mutate(category = factor(category, levels = cat_order))

# ── 4. Labels ─────────────────────────────────────────────────────────────────
labels_rank  <- primary_cat |>
  group_by(category) |>
  slice_min(rank, n = 1, with_ties = FALSE) |>
  ungroup()

labels_rated <- primary_cat |>
  group_by(category) |>
  slice_max(usersrated, n = 1, with_ties = FALSE) |>
  ungroup()

# ── 5. Colour palette (same as series) ────────────────────────────────────────
pal <- c(
  "Abstract Games"      = "#4e79a7",
  "Children's Games"    = "#f28e2b",
  "Customizable Games"  = "#e15759",
  "Family Games"        = "#76b7b2",
  "Party Games"         = "#59a14f",
  "Thematic Games"      = "#edc948",
  "Wargames"            = "#b07aa1",
  "Strategy Games"      = "#ff9da7"
)

# ── 6. Shared violin layer builder ────────────────────────────────────────────
violin_layers <- function() {
  list(
    geom_violin(trim = TRUE, alpha = 0.75, colour = NA, width = 0.85),
    geom_boxplot(width = 0.12, outlier.shape = NA, colour = "grey30",
                 fill = "white", alpha = 0.8, linewidth = 0.4),
    geom_jitter(aes(colour = category), width = 0.18, size = 0.9,
                alpha = 0.5, shape = 16),
    scale_fill_manual(values = pal, guide = "none"),
    scale_colour_manual(values = pal, guide = "none")
  )
}

label_layer <- function(df) {
  geom_label_repel(
    data    = df,
    aes(label = name),
    family  = "lato", size = 15, fontface = "bold",
    fill = "white", colour = "grey20",
    label.size = 0.25, label.padding = unit(0.2, "lines"),
    box.padding = unit(0.5, "lines"), point.padding = unit(0.3, "lines"),
    min.segment.length = 0, max.overlaps = Inf, seed = 42
  )
}

base_theme <- function(show_x = TRUE) {
  list(
    theme_minimal(base_family = "lato", base_size = 13),
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#eeeeee"),
      axis.text.x        = if (show_x)
                             element_text(size = 40, colour = "grey30")
                           else
                             element_blank(),
      axis.ticks.x       = if (show_x) element_line() else element_blank(),
      axis.text.y        = element_text(size = 25),
      plot.margin        = margin(8, 20, 4, 16)
    )
  )
}

# ── 7. Top panel: Bayes-adjusted rating ───────────────────────────────────────
p1 <- ggplot(primary_cat, aes(x = category, y = bayesaverage, fill = category)) +
  violin_layers() +
  label_layer(labels_rank) +
  scale_y_continuous(
    breaks = seq(6.5, 8.5, by = 0.5),
    labels = number_format(accuracy = 0.1)
  ) +
  labs(x = NULL, y = "Bayes-adjusted rating") +
  base_theme(show_x = FALSE)

# ── 8. Bottom panel: number of ratings (popularity proxy) ─────────────────────
p2 <- ggplot(primary_cat, aes(x = category, y = usersrated, fill = category)) +
  violin_layers() +
  label_layer(labels_rated) +
  scale_y_log10(labels = label_comma()) +
  labs(x = NULL, y = "Number of ratings (log scale)") +
  base_theme(show_x = TRUE)

# ── 9. Combine ────────────────────────────────────────────────────────────────
combined <- p1 / p2 +
  plot_annotation(
    title    = "Rating quality vs. popularity across BGG sub-categories",
    subtitle = paste0(
      "Top 500 ranked non-expansion games, by primary sub-category.\n",
      "Top panel: Bayes-adjusted rating (labelled = #1-ranked game). ",
      "Bottom panel: number of user ratings as a popularity proxy (labelled = most-rated game).\n",
      "Boxplot shows median and IQR; log scale used for ratings count."
    ),
    caption  = "Source: BoardGameGeek  |  30 Day Chart Challenge \u2013 Day 22: New Tool",
    theme    = theme(
      plot.title      = element_text(family = "lato", face = "bold", size = 50,
                                      margin = margin(b = 4)),
      plot.subtitle   = element_text(family = "lato", size = 45, colour = "#555555",
                                      lineheight = 0.4, margin = margin(b = 10)),
      plot.caption    = element_text(family = "lato", size = 20, colour = "#888888",
                                      hjust = 1),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(16, 20, 12, 16)
    )
  )

ggsave("images/day22_newtool.png", combined, width = 13, height = 14, dpi = 300)
