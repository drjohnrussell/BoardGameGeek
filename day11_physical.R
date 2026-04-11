library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(scales)
library(stringr)

# Load .Renviron if token not already in environment (e.g. when run from bash)
if (!nzchar(Sys.getenv("BGG_oauth_token"))) readRenviron("~/.Renviron")

base_url  <- "https://boardgamegeek.com/xmlapi2"
bgg_token <- Sys.getenv("BGG_oauth_token")

# ── 1. Top 500 non-expansion games by rank ────────────────────────────────────
games <- read.csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 500)

# Derive primary sub-category for colouring
games <- games |>
  mutate(category = case_when(
    !is.na(strategygames_rank)   ~ "Strategy",
    !is.na(thematic_rank)        ~ "Thematic",
    !is.na(familygames_rank)     ~ "Family",
    !is.na(wargames_rank)        ~ "Wargames",
    !is.na(abstracts_rank)       ~ "Abstract",
    !is.na(partygames_rank)      ~ "Party",
    !is.na(childrensgames_rank)  ~ "Children's",
    !is.na(cgs_rank)             ~ "Customizable",
    TRUE                         ~ "Uncategorised"
  ))

# ── 2. Fetch box dimensions + complexity from API ─────────────────────────────
fetch_physical <- function(ids) {
  req <- request(base_url) |>
    req_url_path_append("thing") |>
    req_url_query(
      id      = paste(ids, collapse = ","),
      type    = "boardgame",
      versions = "1",
      stats   = "1"
    ) |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_retry(max_tries = 3, backoff = \(x) 30)
  if (nzchar(bgg_token)) req <- req_auth_bearer_token(req, bgg_token)

  req |>
    req_perform() |>
    resp_body_xml() |>
    xml_find_all("/items/item[@type='boardgame']") |>
    map_dfr(\(item) {
      game_id <- xml_attr(item, "id")

      # Complexity weight from stats
      avg_weight <- xml_attr(xml_find_first(item, ".//averageweight"), "value") |>
        as.numeric()

      # Box dimensions — take median across versions, ignoring zeros
      versions <- xml_find_all(item, ".//versions/item")
      if (length(versions) > 0) {
        get_val <- function(node, field) {
          x <- xml_attr(xml_find_first(node, paste0(".//", field)), "value")
          v <- suppressWarnings(as.numeric(x))
          if (is.na(v) || v == 0) NA_real_ else v
        }
        widths  <- map_dbl(versions, \(v) get_val(v, "width"))
        lengths <- map_dbl(versions, \(v) get_val(v, "length"))
        depths  <- map_dbl(versions, \(v) get_val(v, "depth"))
        box_w   <- median(widths,  na.rm = TRUE)
        box_l   <- median(lengths, na.rm = TRUE)
        box_d   <- median(depths,  na.rm = TRUE)
      } else {
        box_w <- box_l <- box_d <- NA_real_
      }

      tibble(
        game_id    = game_id,
        avg_weight = avg_weight,
        box_w      = box_w,
        box_l      = box_l,
        box_d      = box_d
      )
    })
}

cache_file <- "physical_raw.rds"
if (file.exists(cache_file)) {
  message("Loading cached physical data...")
  physical_raw <- readRDS(cache_file)
} else {
  message("Cache not found — fetching from BGG API...")
  batches <- split(games$id, ceiling(seq_along(games$id) / 20))
  physical_raw <- map_dfr(batches, \(batch) {
    message("Fetching batch of ", length(batch), " games...")
    res <- fetch_physical(batch)
    Sys.sleep(2)
    res
  })
  saveRDS(physical_raw, cache_file)
}

# ── 3. Join and clean ─────────────────────────────────────────────────────────
plot_data <- physical_raw |>
  mutate(game_id = as.integer(game_id)) |>
  left_join(games |> select(id, name, rank, category), by = c("game_id" = "id")) |>
  filter(
    !is.na(avg_weight), avg_weight > 0,
    !is.na(box_w), !is.na(box_l),
    # BGG stores dimensions in inches; filter implausible values
    box_w > 2, box_w < 40,
    box_l > 2, box_l < 40
  ) |>
  mutate(
    footprint = box_w * box_l,   # square inches
    # Flag notable outliers to label
    label = case_when(
      # Tiny but heavy
      footprint < 50  & avg_weight >= 3.5 ~ name,
      # Giant and heavy
      footprint > 200 & avg_weight >= 4.0 ~ name,
      # Giant but light
      footprint > 220 & avg_weight < 2.5  ~ name,
      # Top-ranked + interesting position
      rank <= 5                            ~ name,
      TRUE                                 ~ NA_character_
    )
  )

# ── 4. Category colours ───────────────────────────────────────────────────────
cat_colours <- c(
  "Strategy"      = "#E63946",
  "Thematic"      = "#F4A261",
  "Family"        = "#2A9D8F",
  "Wargames"      = "#6A4E23",
  "Abstract"      = "#457B9D",
  "Party"         = "#E9C46A",
  "Children's"    = "#A8DADC",
  "Customizable"  = "#9B5DE5",
  "Uncategorised" = "grey70"
)

# ── 5. Crokinole annotation data ──────────────────────────────────────────────
crokinole <- plot_data |> filter(grepl("Crokinole", name, ignore.case = TRUE))

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(plot_data |> filter(footprint <= 300),
       aes(x = avg_weight, y = footprint, colour = category)) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(
    aes(group = 1), method = "loess", se = TRUE,
    colour = "grey30", fill = "grey85", linewidth = 0.7, alpha = 0.4
  ) +
  geom_label_repel(
    aes(label = label),
    size = 2.8, label.padding = 0.2, label.size = 0.2,
    max.overlaps = 20, seed = 42,
    colour = "grey15", fill = alpha("white", 0.85)
  ) +
  # Arrow + label for off-scale Crokinole
  annotate(
    "segment",
    x = crokinole$avg_weight+.25, xend = crokinole$avg_weight+.125,
    y = 285, yend = 300,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = "#457B9D", linewidth = 0.7
  ) +
  annotate(
    "label",
    x = crokinole$avg_weight+.25, y = 275,
    label = paste0("Crokinole\n", round(crokinole$footprint), " in\u00b2"),
    size = 2.6, colour = "#457B9D", fill = alpha("white", 0.85),
    label.size = 0.2, label.padding = unit(0.2, "lines")
  ) +
  scale_colour_manual(values = cat_colours, name = "Sub-category") +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("1\n(Light)", "2", "3", "4", "5\n(Heavy)"),
    limits = c(1, 5)
  ) +
  scale_y_continuous(
    labels = \(x) paste0(round(x), " in\u00b2"),
    breaks = seq(0, 300, 50),
    limits = c(0, 300)
  ) +
  labs(
    title    = "Does Complexity Come in a Big Box?",
    subtitle = paste0(
      "Box footprint (width \u00d7 length) vs. complexity weight for\n",
      "BGG\u2019s top 500 ranked games"
    ),
    x       = "Complexity weight (BGG community rating)",
    y       = "Box footprint (sq inches, median across editions)",
    caption = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 11: Physical"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 22, colour = "grey10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10.5, lineheight = 1.3,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "grey92", linewidth = 0.4),
    axis.text          = element_text(size = 9, colour = "grey45"),
    axis.title         = element_text(size = 10, colour = "grey35"),
    legend.position    = "right",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 8.5),
    legend.key.size    = unit(0.45, "cm"),
    plot.margin        = margin(15, 20, 15, 15),
    plot.background    = element_rect(fill = "grey98", colour = NA)
  )

ggsave("images/day11_physical.png", width = 12, height = 8, dpi = 300)
