library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()

base_url  <- "https://boardgamegeek.com/xmlapi2"
bgg_token <- Sys.getenv("BGG_oauth_token")
username  <- "Messier104"

# ── 1. Fetch all play pages ───────────────────────────────────────────────────
parse_page <- function(resp_xml) {
  plays <- xml_find_all(resp_xml, ".//play")
  if (length(plays) == 0) return(NULL)
  tibble(
    date     = xml_attr(plays, "date"),
    quantity = as.integer(xml_attr(plays, "quantity")),
    game_id  = map_chr(plays, ~ xml_attr(xml_find_first(.x, ".//item"), "objectid")),
    game     = map_chr(plays, ~ xml_attr(xml_find_first(.x, ".//item"), "name"))
  )
}

make_req <- function(pg) {
  request(base_url) |>
    req_url_path_append("plays") |>
    req_url_query(username = username, page = pg) |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_auth_bearer_token(bgg_token) |>
    req_retry(max_tries = 3, backoff = \(x) 10) |>
    req_perform() |>
    resp_body_xml()
}

resp1   <- make_req(1)
total   <- as.integer(xml_attr(xml_root(resp1), "total"))
n_pages <- ceiling(total / 100)
message(sprintf("Total plays: %d  |  Pages to fetch: %d", total, n_pages))

if (file.exists("plays_raw.rds")) {
  plays_raw <- readRDS("plays_raw.rds")
  message("Loaded plays from cache.")
} else {
  plays_raw <- map_dfr(seq_len(n_pages), function(pg) {
    if (pg > 1) Sys.sleep(2)
    resp <- if (pg == 1) resp1 else make_req(pg)
    parse_page(resp)
  })
  saveRDS(plays_raw, "plays_raw.rds")
  message("Plays fetched and cached.")
}

# ── 2. Parse dates & assign seasons ──────────────────────────────────────────
month_abbr <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

plays_df <- plays_raw |>
  filter(!is.na(date), nchar(date) == 10) |>
  mutate(
    date_parsed = as.Date(date),
    month_num   = as.integer(format(date_parsed, "%m")),
    year        = as.integer(format(date_parsed, "%Y")),
    quantity    = coalesce(quantity, 1L)
  ) |>
  filter(!is.na(date_parsed)) |>
  mutate(season = case_when(
    month_num %in% c(12, 1, 2) ~ "Winter",
    month_num %in% c(3,  4, 5) ~ "Spring",
    month_num %in% c(6,  7, 8) ~ "Summer",
    TRUE                        ~ "Autumn"
  ))

# ── 3. Aggregate by month ─────────────────────────────────────────────────────
monthly <- plays_df |>
  group_by(month_num, season) |>
  summarise(plays = sum(quantity), .groups = "drop") |>
  mutate(
    month_name = factor(month_abbr[month_num], levels = month_abbr),
    season     = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# ── 4. Palette ────────────────────────────────────────────────────────────────
pal <- c(
  "Winter" = "#7eb0d5",
  "Spring" = "#74c476",
  "Summer" = "#f6c85f",
  "Autumn" = "#d4735a"
)

inner        <- max(monthly$plays) * 0.38
total_logged <- sum(monthly$plays)
yr_range     <- paste(min(plays_df$year), "\u2013", max(plays_df$year))

# Season mid-month for quadrant labels (positioned inside hole)
season_ann <- tibble(
  month_name = factor(c("Jan", "Apr", "Jul", "Oct"), levels = month_abbr),
  y          = -inner * 0.6,
  label      = c("WINTER", "SPRING", "SUMMER", "AUTUMN"),
  season     = factor(c("Winter", "Spring", "Summer", "Autumn"),
                      levels = c("Winter", "Spring", "Summer", "Autumn"))
)

# ── 5. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(monthly, aes(x = month_name, y = plays, fill = season)) +
  geom_col(width = 0.82, colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = plays, y = plays + max(plays) * 0.04),
    family = "lato", size = 15, colour = "grey25", fontface = "bold"
  ) +
  geom_text(
    data = season_ann,
    aes(x = month_name, y = y, label = label, colour = season),
    family = "lato", size = 15, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_colour_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(-inner, NA), expand = c(0, 0)) +
  coord_polar(start = 0, direction = 1) +
  labs(
    title    = "When during the year do I play board games?",
    subtitle = paste0("Plays logged on BoardGameGeek by month  \u2022  ", yr_range,
                      "  \u2022  ", scales::comma(total_logged), " total plays"),
    x        = NULL,
    caption  = "Source: BoardGameGeek API  |  30 Day Chart Challenge \u2013 Day 23: Seasons"
  ) +
  theme_minimal(base_family = "lato", base_size = 20) +
  theme(
    plot.title       = element_text(face = "bold", size = 70, hjust = 0.5,
                                     margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 50, colour = "#555555", hjust = 0.5,
                                     margin = margin(b = 8)),
    plot.caption     = element_text(size = 40, colour = "#888888", hjust = 1,
                                     margin = margin(t = 8)),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text.x      = element_text(size = 40, colour = "grey40", family = "lato",
                                     margin = margin(t = 2)),
    axis.text.y      = element_blank(),
    axis.ticks       = element_blank(),
    plot.margin      = margin(16, 16, 12, 16)
  )

ggsave("images/day23_seasons.png", p, width = 10, height = 10, dpi = 300)
