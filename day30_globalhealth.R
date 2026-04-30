library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(scales)
library(showtext)

font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
df_raw <- read.csv(
  "data/IHME_PREM_EDUCATION_2021_DATA/IHME_PREM_EDUCATION_2021_Y2021M09D14.CSV",
  na.strings = c("NA", "")
)

# ---------------------------------------------------------------------------
# Pivot child internet access columns long
# internet_access: 0=Never, 1=Rarely, 2=Usually, 3=Most of time, 4=Always
# Only filled when a child is doing remote/hybrid learning
# ---------------------------------------------------------------------------
child_access <- df_raw |>
  select(country, geography,
         edu_ch1_internet_access, edu_ch2_internet_access,
         edu_ch3_internet_access, edu_ch4_internet_access) |>
  pivot_longer(
    cols      = starts_with("edu_ch"),
    names_to  = "child",
    values_to = "internet_access"
  ) |>
  filter(!is.na(internet_access))

# ---------------------------------------------------------------------------
# Panel A data: % with no or rare access (0–1) by country
# ---------------------------------------------------------------------------
map_data <- child_access |>
  group_by(country) |>
  summarise(
    n           = n(),
    pct_limited = mean(internet_access <= 1) * 100,
    .groups     = "drop"
  ) |>
  filter(n >= 20) |>
  mutate(country_ne = case_when(
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United Republic of Tanzania"        ~ "Tanzania",
    country == "Democratic Republic of the Congo"   ~ "Dem. Rep. Congo",
    country == "Republic of Moldova"                ~ "Moldova",
    country == "United States"                      ~ "United States of America",
    TRUE                                            ~ country
  ))

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, geometry)

map_sf <- world |>
  left_join(map_data, by = c("name" = "country_ne"))

# ---------------------------------------------------------------------------
# Panel B data: city vs rural "strong access" (3–4 days/wk) by country
# geography: 1=City/metro, 2=Suburban, 3=Rural
# ---------------------------------------------------------------------------
dumbbell_data <- child_access |>
  filter(geography %in% c(1, 3)) |>
  mutate(
    geo_label     = if_else(geography == 1, "City", "Rural"),
    strong_access = internet_access >= 3
  ) |>
  group_by(country, geo_label) |>
  summarise(pct = mean(strong_access) * 100, n = n(), .groups = "drop") |>
  filter(n >= 20) |>
  pivot_wider(names_from = geo_label, values_from = c(pct, n)) |>
  filter(!is.na(pct_City), !is.na(pct_Rural)) |>
  mutate(gap = pct_City - pct_Rural) |>
  arrange(desc(abs(gap))) |>
  slice_head(n = 22) |>
  arrange(gap) |>
  mutate(
    country_label = case_when(
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      country == "United Republic of Tanzania"        ~ "Tanzania",
      country == "Democratic Republic of the Congo"   ~ "DR Congo",
      country == "Dominican Republic"                 ~ "Dominican Rep.",
      TRUE                                            ~ country
    ),
    country_label = factor(country_label, levels = country_label)
  )

db_long <- dumbbell_data |>
  select(country_label, pct_City, pct_Rural) |>
  pivot_longer(
    cols      = c(pct_City, pct_Rural),
    names_to  = "geo",
    values_to = "pct"
  ) |>
  mutate(geo = if_else(geo == "pct_City", "City / metro", "Rural"),
         geo = factor(geo, levels = c("Rural", "City / metro")))

# ---------------------------------------------------------------------------
# Shared theme elements
# ---------------------------------------------------------------------------
map_theme <- theme_void(base_family = "roboto") +
  theme(
    plot.title      = element_text(family = "roboto_condensed", face = "bold",
                                   size = 22, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 16, color = "#555555",
                                   lineheight = 0.5, margin = margin(b = 6)),
    legend.position  = "bottom",
    legend.title     = element_text(size = 16, face = "bold"),
    legend.text      = element_text(size = 14),
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    plot.margin      = margin(6, 12, 6, 12)
  )

# ---------------------------------------------------------------------------
# Panel A — World choropleth
# ---------------------------------------------------------------------------
p_map <- ggplot(map_sf) +
  geom_sf(aes(fill = pct_limited), colour = "white", linewidth = 0.08) +
  scale_fill_gradientn(
    colours  = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", "#08306B"),
    values   = rescale(c(0, 20, 40, 65, 100)),
    na.value = "#E0E0E0",
    name     = "% with no or rare internet access for learning",
    limits   = c(0, 100),
    breaks   = c(0, 25, 50, 75, 100),
    labels   = c("0%", "25%", "50%", "75%", "100%"),
    guide    = guide_colourbar(
      title.position = "top", title.hjust = 0.5, ticks = FALSE
    )
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(
    title    = "Who couldn't connect?",
    subtitle = "Share of remote-learning children with never or rarely reliable\ninternet access during COVID-19 school closures  (grey = not surveyed)"
  ) +
  map_theme

# ---------------------------------------------------------------------------
# Panel B — Dumbbell: city vs rural
# ---------------------------------------------------------------------------
p_dumbbell <- ggplot() +
  geom_segment(
    data    = dumbbell_data,
    aes(x = pct_Rural, xend = pct_City, y = country_label, yend = country_label),
    colour  = "#BBBBBB",
    linewidth = 1
  ) +
  geom_point(
    data  = db_long,
    aes(x = pct, y = country_label, colour = geo),
    size  = 3.5
  ) +
  scale_colour_manual(
    values = c("City / metro" = "#2171B5", "Rural" = "#FC8D59"),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title    = "The within-country gap",
    subtitle = "% of remote-learning children with strong internet access (5+ days/week)\nby location of household  — ordered by city–rural gap",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title      = element_text(family = "roboto_condensed", face = "bold",
                                   size = 22, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 16, color = "#555555",
                                   lineheight = 0.5, margin = margin(b = 8)),
    legend.position  = "top",
    legend.justification = "left",
    legend.text      = element_text(size = 16),
    legend.key.size  = unit(0.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#F2F2F2"),
    panel.grid.major.x = element_line(colour = "#E8E8E8"),
    axis.text.y      = element_text(size = 14),
    axis.text.x      = element_text(size = 13, color = "#666666"),
    plot.margin      = margin(6, 16, 6, 12)
  )

# ---------------------------------------------------------------------------
# Combine with patchwork
# ---------------------------------------------------------------------------
p_final <- p_map / p_dumbbell +
  plot_layout(heights = c(1.2, 1.8)) +
  plot_annotation(
    title    = "COVID-19 and the Education Digital Divide",
    subtitle = paste0(
      "Across 35 countries, reliable internet access determined who could learn during pandemic school closures.\n",
      "The gap between city and rural households was especially stark in lower-income nations."
    ),
    caption  = paste0(
      "Source: IHME Program on Reproducible Epidemiology Modeling (PREM) Education Survey, 2021  ",
      "|  30 Day Chart Challenge – Day 30: Global Health Data Exchange"
    ),
    theme = theme(
      plot.title    = element_text(family = "roboto_condensed", face = "bold",
                                   size = 34, margin = margin(b = 5)),
      plot.subtitle = element_text(family = "roboto", size = 20,
                                   color = "#444444", lineheight = 0.5,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(family = "roboto", size = 15,
                                   color = "#888888", margin = margin(t = 10)),
      plot.margin   = margin(18, 18, 14, 18)
    )
  )

ggsave("images/day30_globalhealth.png", p_final,
       width = 13, height = 17, dpi = 300, bg = "white")
