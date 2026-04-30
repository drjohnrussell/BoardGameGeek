library(tidyverse)
library(scales)
library(showtext)
library(ggrepel)
library(httr2)
library(xml2)

font_add_google("Playfair Display", "playfair")
font_add_google("Roboto", "roboto")
showtext_auto()

bgg_token <- Sys.getenv("BGG_oauth_token")

# ── Data ──────────────────────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv", show_col_types = FALSE) |>
  filter(is_expansion == 0, rank > 0)

top500 <- ranks |> arrange(rank) |> slice_head(n = 500)

# ── API fetch: stddev for top 500 games ───────────────────────────────────────
cache_file <- "stddev_raw.rds"

if (!file.exists(cache_file)) {
  ids     <- top500$id
  batches <- split(ids, ceiling(seq_along(ids) / 20))

  stddev_data <- map_dfr(seq_along(batches), function(i) {
    resp <- request("https://boardgamegeek.com/xmlapi2/thing") |>
      req_url_query(id = paste(batches[[i]], collapse = ","),
                    type = "boardgame", stats = 1) |>
      req_auth_bearer_token(bgg_token) |>
      req_perform() |>
      resp_body_xml()

    items <- xml_find_all(resp, "/items/item[@type='boardgame']")

    batch_df <- map_dfr(items, function(item) {
      tibble(
        game_id    = xml_attr(item, "id"),
        name       = xml_attr(xml_find_first(item, ".//name[@type='primary']"), "value"),
        stddev     = as.numeric(xml_attr(xml_find_first(item, ".//stddev"),     "value")),
        usersrated = as.integer(xml_attr(xml_find_first(item, ".//usersrated"), "value"))
      )
    })

    message("Batch ", i, "/", length(batches), " done")
    if (i < length(batches)) Sys.sleep(2)
    batch_df
  })

  saveRDS(stddev_data, cache_file)
} else {
  stddev_data <- readRDS(cache_file)
}

# ── Derive primary sub-category from ranks ────────────────────────────────────
sub_cat <- top500 |>
  mutate(
    game_id  = as.character(id),
    category = case_when(
      !is.na(strategygames_rank)  ~ "Strategy",
      !is.na(thematic_rank)       ~ "Thematic",
      !is.na(wargames_rank)       ~ "Wargames",
      !is.na(familygames_rank)    ~ "Family",
      !is.na(abstracts_rank)      ~ "Abstracts",
      !is.na(partygames_rank)     ~ "Party",
      !is.na(childrensgames_rank) ~ "Children's",
      !is.na(cgs_rank)            ~ "Customizable",
      TRUE                        ~ "Other"
    )
  ) |>
  select(game_id, bayesaverage, category)

# ── Join ──────────────────────────────────────────────────────────────────────
plot_data <- stddev_data |>
  left_join(sub_cat, by = "game_id") |>
  filter(!is.na(stddev), usersrated >= 500)

# Top 5 most divisive overall + top 5 most divisive among highly-rated games
label_games <- bind_rows(
  plot_data |> slice_max(stddev, n = 5),
  plot_data |> filter(bayesaverage >= 8.0) |> slice_max(stddev, n = 5)
) |> distinct(game_id, .keep_all = TRUE)

mean_sd <- mean(plot_data$stddev)

# ── Palette ───────────────────────────────────────────────────────────────────
cat_colors <- c(
  "Strategy"     = "#1F78B4",
  "Thematic"     = "#E31A1C",
  "Wargames"     = "#8B4513",
  "Family"       = "#33A02C",
  "Abstracts"    = "#6A3D9A",
  "Party"        = "#FF7F00",
  "Children's"   = "#FB9A99",
  "Customizable" = "#B2DF8A",
  "Other"        = "#999999"
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(plot_data, aes(x = bayesaverage, y = stddev)) +
  geom_hline(yintercept = mean_sd, linetype = "dashed",
             colour = "#BBBBBB", linewidth = 0.5) +
  annotate("text", x = 7.52, y = mean_sd + 0.025,
           label = "← avg std dev",
           size = 7, colour = "#AAAAAA", hjust = 0, family = "roboto") +
  geom_point(aes(colour = category, size = usersrated),
             alpha = 0.65, shape = 16) +
  geom_label_repel(
    data         = label_games,
    aes(label    = name, colour = category),
    family       = "roboto",
    size         = 4.5,
    fontface     = "bold",
    fill         = alpha("white", 0.85),
    label.size   = 0,
    box.padding  = 0.5,
    max.overlaps = 25,
    show.legend  = FALSE
  ) +
  scale_colour_manual(values = cat_colors, name = "Sub-category") +
  scale_size_continuous(
    name   = "# ratings",
    range  = c(1.5, 8),
    labels = label_comma(),
    breaks = c(10000, 50000, 100000, 200000)
  ) +
  scale_x_continuous(breaks = seq(7.5, 8.5, 0.25), limits = c(7.45, NA)) +
  labs(
    title    = "Beloved and Divisive",
    subtitle = "Standard deviation of user ratings vs Bayes-adjusted score for the top 500 BGG games.\nGames higher up are polarising — players strongly love or hate them. Lower games have near-universal consensus.",
    x        = "Bayes-adjusted rating",
    y        = "Std dev of ratings  (divisiveness →)",
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge – Day 31: Divisiveness"
  ) +
  theme_minimal(base_family = "roboto", base_size = 18) +
  theme(
    plot.title       = element_text(family = "playfair", face = "bold", size = 44,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 22, colour = "#444444", lineheight = 0.45,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 14, colour = "#888888", margin = margin(t = 12)),
    plot.margin      = margin(18, 20, 12, 18),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#F0F0F0"),
    legend.position  = "right",
    legend.title     = element_text(size = 16),
    legend.text      = element_text(size = 14)
  )

ggsave("images/day31_divisiveness.png", p,
       width = 13, height = 10, dpi = 300, bg = "white")
