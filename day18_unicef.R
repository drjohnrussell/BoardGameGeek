library(readxl)
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
# Load & reshape UNICEF data
# ---------------------------------------------------------------------------
df_raw <- read_excel("data/Under-five_Mortality_Rates_2025-1.xlsx", sheet = 1, skip = 13)

df_2024 <- df_raw |>
  rename(uncertainty = `Uncertainty.Bounds*`) |>
  select(ISO.Code, Country.Name, SDG.Region, uncertainty, `2024.5`) |>
  pivot_wider(names_from = uncertainty, values_from = `2024.5`) |>
  mutate(
    ci_pct = (Upper - Lower) / Median * 100,
    # Cap at 300% for display purposes (Andorra hits 380% — tiny population artefact)
    ci_pct_capped = pmin(ci_pct, 300)
  )

# ---------------------------------------------------------------------------
# World geometries
# ---------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# rnaturalearth uses iso_a3; a few territories use "-99" — fix known ones
world <- world |>
  mutate(iso_a3 = case_when(
    name == "Kosovo"         ~ "XKX",
    name == "France"         ~ "FRA",
    iso_a3 == "-99"          ~ iso_a3_eh,   # fallback to EH codes
    TRUE                     ~ iso_a3
  ))

map_df <- world |>
  left_join(df_2024, by = c("iso_a3" = "ISO.Code"))

# ---------------------------------------------------------------------------
# Shared theme
# ---------------------------------------------------------------------------
map_theme <- theme_void(base_family = "roboto") +
  theme(
    plot.title        = element_text(family = "roboto_condensed", face = "bold",
                                     size = 25, margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 20, color = "#555555",
                                     margin = margin(b = 4)),
    legend.position   = "bottom",
    legend.title      = element_text(size = 20, face = "bold"),
    legend.text       = element_text(size = 18),
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    plot.margin       = margin(8, 8, 8, 8)
  )

# ---------------------------------------------------------------------------
# Panel 1 — U5MR choropleth
# ---------------------------------------------------------------------------
p_mortality <- ggplot(map_df) +
  geom_sf(aes(fill = Median), colour = "white", linewidth = 0.08) +
  scale_fill_gradientn(
    colours   = c("#FFF5F0", "#FCBBA1", "#FC7050", "#CB181D", "#67000D"),
    values    = rescale(c(0, 15, 40, 80, 120)),
    na.value  = "#DDDDDD",
    name      = "Deaths per 1,000 live births",
    breaks    = c(5, 20, 40, 80, 115),
    labels    = c("5", "20", "40", "80", "115"),
    guide     = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                ticks = FALSE)
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(
    title    = "Where children are most at risk",
    subtitle = "Under-five mortality rate, 2024 (median estimate)"
  ) +
  map_theme

# ---------------------------------------------------------------------------
# Panel 2 — Data uncertainty choropleth
# ---------------------------------------------------------------------------
p_uncertainty <- ggplot(map_df) +
  geom_sf(aes(fill = ci_pct_capped), colour = "white", linewidth = 0.08) +
  scale_fill_gradientn(
    colours  = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    values   = rescale(c(0, 30, 80, 150, 300)),
    na.value = "#DDDDDD",
    name     = "Upper/Lower Bound width as % of median estimate",
    breaks   = c(0, 50, 100, 200, 300),
    labels   = c("0%", "50%", "100%", "200%", "≥300%"),
    guide    = guide_colourbar(title.position = "top", title.hjust = 0.5,
                               ticks = FALSE)
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(
    title    = "Where the data is weakest",
    subtitle = "Uncertainty in estimates, 2024  —  darker = less reliable vital registration"
  ) +
  map_theme

# ---------------------------------------------------------------------------
# Combine
# ---------------------------------------------------------------------------
p_final <- p_mortality / p_uncertainty +
  plot_annotation(
    title    = "Child Mortality: The Known and the Uncertain",
    subtitle = "The places where children face the greatest risk are often the same places where civil registration\nis weakest and mortality estimates carry the most uncertainty.  Grey countries = not in UNICEF dataset.",
    caption  = "Source: UN IGME / UNICEF Global Databases (data.unicef.org), March 2026  |  30 Day Chart Challenge \u2013 Day 18: UNICEF",
    theme = theme(
      plot.title    = element_text(family = "roboto_condensed", face = "bold",
                                   size = 30, margin = margin(b = 3)),
      plot.subtitle = element_text(family = "roboto", size = 20,
                                   color = "#444444", lineheight = 0.5,
                                   margin = margin(b = 3)),
      plot.caption  = element_text(family = "roboto", size = 15,
                                   color = "#888888", margin = margin(t = 6)),
      plot.margin   = margin(16, 16, 12, 16)
    )
  )

ggsave("images/day18_unicef.png", p_final,
       width = 8, height = 10, dpi = 300, bg = "white")
