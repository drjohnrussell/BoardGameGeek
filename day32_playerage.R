library(tidyverse)
library(scales)
library(showtext)
library(httr2)
library(xml2)

font_add_google("Libre Baskerville", "baskerville")
font_add_google("Source Sans 3",     "sourcesans")
showtext_auto()

bgg_token <- Sys.getenv("BGG_oauth_token")

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0)

top500 <- ranks |> arrange(rank) |> slice_head(n = 500)

# ── API fetch: suggested_playerage poll + publisher minage ────────────────────
cache_file <- "playerage_raw.rds"

if (!file.exists(cache_file)) {
  ids     <- top500$id
  batches <- split(ids, ceiling(seq_along(ids) / 20))

  age_data <- map_dfr(seq_along(batches), function(i) {
    resp <- request("https://boardgamegeek.com/xmlapi2/thing") |>
      req_url_query(id = paste(batches[[i]], collapse = ","), type = "boardgame") |>
      req_auth_bearer_token(bgg_token) |>
      req_perform() |>
      resp_body_xml()

    items <- xml_find_all(resp, "/items/item[@type='boardgame']")

    batch_df <- map_dfr(items, function(item) {
      game_id    <- xml_attr(item, "id")
      minage     <- as.integer(xml_attr(xml_find_first(item, ".//minage"), "value"))
      poll       <- xml_find_first(item, ".//poll[@name='suggested_playerage']")
      if (is.na(poll)) return(NULL)
      totalvotes <- as.integer(xml_attr(poll, "totalvotes"))
      if (is.na(totalvotes) || totalvotes < 5) return(NULL)

      result_nodes <- xml_find_all(poll, ".//result")
      tibble(
        game_id    = game_id,
        minage     = minage,
        totalvotes = totalvotes,
        age        = xml_attr(result_nodes, "value"),
        numvotes   = as.integer(xml_attr(result_nodes, "numvotes"))
      )
    })

    message("Batch ", i, "/", length(batches), " done")
    if (i < length(batches)) Sys.sleep(2)
    batch_df
  })

  saveRDS(age_data, cache_file)
} else {
  age_data <- readRDS(cache_file)
}

# ── Age levels ────────────────────────────────────────────────────────────────
age_thresholds <- c(6, 8, 10, 12, 14, 16, 18, 21)
age_levels     <- c("6","8","10","12","14","16","18","21 and up")
age_labels     <- c("6","8","10","12","14","16","18","21+")

# Snap a numeric minage to the nearest poll age level label
snap_age <- function(x) {
  idx <- which.min(abs(age_thresholds - x))
  age_labels[idx]
}

# ── Plot data: top 50 games ───────────────────────────────────────────────────
top50 <- top500 |>
  slice_head(n = 50) |>
  mutate(
    game_id    = as.character(id),
    label_name = paste0("#", rank, "  ", name)
  )

plot_data <- age_data |>
  filter(game_id %in% top50$game_id) |>
  left_join(top50 |> select(game_id, label_name), by = "game_id") |>
  mutate(age = factor(age, levels = age_levels, labels = age_labels)) |>
  filter(!is.na(age)) |>
  group_by(game_id) |>
  mutate(pct = numvotes / sum(numvotes)) |>
  ungroup() |>
  mutate(label_name = factor(label_name, levels = rev(top50$label_name)))

# Publisher minage — snap to nearest poll age for x-axis alignment
minage_data <- age_data |>
  filter(game_id %in% top50$game_id) |>
  select(game_id, minage) |>
  distinct() |>
  left_join(top50 |> select(game_id, label_name), by = "game_id") |>
  filter(!is.na(minage), minage > 0) |>
  mutate(
    label_name  = factor(label_name, levels = rev(top50$label_name)),
    age_snapped = factor(map_chr(minage, snap_age), levels = age_labels)
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(plot_data, aes(x = age, y = label_name, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(
    data        = minage_data,
    aes(x       = age_snapped, y = label_name),
    inherit.aes = FALSE,
    shape       = 18,
    colour      = "#E74C3C",
    size        = 3.5
  ) +
  scale_fill_gradient(
    low      = "#FFF5EB",
    high     = "#7F2704",
    na.value = "grey93",
    limits   = c(0, 1),
    labels   = percent_format(accuracy = 1),
    name     = "Share of\nvotes"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "How Old Should You Be?",
    subtitle = "Share of BGG community votes for each minimum age recommendation, top 50 ranked games.\nDarker = stronger consensus for that age. Red diamond ◆ = publisher's stated minimum age.",
    x        = "Community-recommended minimum age",
    y        = NULL,
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 32: Recommended Age"
  ) +
  theme_minimal(base_family = "sourcesans", base_size = 18) +
  theme(
    plot.title    = element_text(family = "baskerville", face = "bold", size = 44,
                                 margin = margin(b = 6)),
    plot.subtitle = element_text(size = 22, colour = "#444444", lineheight = 0.45,
                                 margin = margin(b = 10)),
    plot.caption  = element_text(size = 14, colour = "#888888", margin = margin(t = 12)),
    plot.margin   = margin(18, 20, 12, 18),
    panel.grid    = element_blank(),
    axis.text.y   = element_text(size = 17, colour = "#333333", hjust = 1),
    axis.text.x   = element_text(size = 18, colour = "#444444", face = "bold"),
    axis.title.x  = element_text(size = 16, margin = margin(b = 6)),
    legend.title  = element_text(size = 16, lineheight = 0.4),
    legend.text   = element_text(size = 14)
  )

ggsave("images/day32_playerage.png", p,
       width = 13, height = 15, dpi = 300, bg = "white")
