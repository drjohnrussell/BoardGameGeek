library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)

# ── 1. Load data ──────────────────────────────────────────────────────────────
rwb <- read_delim(
  "data/2025 Reporters without Borders.csv",
  delim     = ";",
  locale    = locale(decimal_mark = ",", encoding = "latin1"),
  show_col_types = FALSE
) |>
  rename(
    political   = `Political Context`,
    safety      = Safety,
    zone        = Zone,
    country     = Country_EN
  ) |>
  filter(!is.na(political), !is.na(safety))

# ── 2. Flag countries to label ────────────────────────────────────────────────
# Divergence: positive = physically safer than political climate suggests
rwb <- rwb |>
  mutate(divergence = safety - political)

label_countries <- bind_rows(
  rwb |> slice_min(safety,     n = 5),           # most physically dangerous
  rwb |> slice_max(divergence, n = 6),            # safer than politics predicts
  rwb |> slice_min(divergence, n = 6),            # more dangerous than politics predicts
  rwb |> filter(country %in% c(
    "United States", "Russia", "China", "North Korea",
    "United Kingdom", "France", "India", "Mexico", "Brazil"
  ))
) |>
  distinct(ISO, .keep_all = TRUE)

# ── 3. Plot ───────────────────────────────────────────────────────────────────
ggplot(rwb, aes(x = political, y = safety, colour = zone)) +

  # Reference diagonal — where Safety = Political Context
  geom_abline(
    slope     = 1, intercept = 0,
    colour    = "grey65", linetype = "dashed", linewidth = 0.55
  ) +

  # All points
  geom_point(alpha = 0.75, size = 2.4) +

  # Labels for notable countries
  geom_text_repel(
    data           = label_countries,
    aes(label      = country),
    size           = 2.7,
    fontface       = "bold",
    box.padding    = 0.45,
    max.overlaps   = 30,
    segment.colour = "grey55",
    segment.size   = 0.3,
    show.legend    = FALSE
  ) +

  # Diagonal annotations
  annotate(
    "text", x = 5, y = 97,
    label    = "Safety > Political context",
    colour   = "grey45", size = 3.2, hjust = 0, fontface = "italic"
  ) +
  annotate(
    "text", x = 52, y = 8,
    label    = "Safety < Political context",
    colour   = "grey45", size = 3.2, hjust = 0, fontface = "italic"
  ) +

  scale_colour_brewer(palette = "Dark2", name = "Region") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  coord_equal(clip = "off") +
  labs(
    title    = "Physical safety vs. political climate for journalists",
    subtitle = paste0(
      "Each point is a country. The dashed diagonal marks where Safety score = Political Context score.\n",
      "Above the line: journalists are physically safer than the political environment suggests.\n",
      "Below the line: physical danger outpaces what political context alone predicts."
    ),
    caption  = "Source: Reporters Without Borders 2025  |  30 Day Chart Challenge \u2013 Day 5: Reporters without Borders",
    x        = "Political Context score",
    y        = "Safety score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey60", size = 8, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.margin      = margin(15, 15, 15, 15)
  )

ggsave("images/day05_scatter.png", width = 10, height = 9, dpi = 300)
