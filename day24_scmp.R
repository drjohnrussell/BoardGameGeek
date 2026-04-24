library(tidyverse)
library(voronoiTreemap)
library(V8)
library(showtext)
library(scales)

font_add_google("Playfair Display", "playfair")
font_add_google("Source Sans 3",    "sourcesans")
showtext_auto()

# ── 1. Load & prep data ───────────────────────────────────────────────────────
ranks <- read_csv("data/boardgames_ranks.csv") |>
  filter(is_expansion == 0, rank > 0) |>
  arrange(rank) |>
  slice_head(n = 200)

sub_cat_map <- c(
  abstracts_rank      = "Abstract Games",
  cgs_rank            = "Customizable",
  childrensgames_rank = "Children's Games",
  familygames_rank    = "Family Games",
  partygames_rank     = "Party Games",
  strategygames_rank  = "Strategy Games",
  thematic_rank       = "Thematic Games",
  wargames_rank       = "Wargames"
)

cat_pal <- c(
  "Strategy Games"   = "#C0392B",
  "Thematic Games"   = "#E67E22",
  "Abstract Games"   = "#2980B9",
  "Family Games"     = "#27AE60",
  "Wargames"         = "#8E44AD",
  "Party Games"      = "#F39C12",
  "Children's Games" = "#1ABC9C",
  "Customizable"     = "#7F8C8D",
  "Uncategorized"    = "#95A5A6"
)

games <- ranks |>
  rowwise() |>
  mutate(
    primary_cat = {
      vals <- c(abstracts_rank, cgs_rank, childrensgames_rank, familygames_rank,
                partygames_rank, strategygames_rank, thematic_rank, wargames_rank)
      names(vals) <- names(sub_cat_map)
      if (all(is.na(vals))) "Uncategorized"
      else unname(sub_cat_map[names(which.min(vals))])
    }
  ) |>
  ungroup()

# ── 2. Build node tree & export JSON ─────────────────────────────────────────
vt_inp <- games |>
  mutate(
    h1     = "All",
    h2     = primary_cat,
    h3     = name,
    color  = cat_pal[primary_cat],
    weight = usersrated,
    codes  = name
  ) |>
  select(h1, h2, h3, color, weight, codes) |>
  as.data.frame()

n      <- vt_input_from_df(vt_inp, scaleToPerc = FALSE)
n_json <- vt_export_json(n)

# ── 3. Run D3 Voronoi treemap computation in V8 ───────────────────────────────
pkg_js <- system.file("htmlwidgets/lib/d3", package = "voronoiTreemap")

ctx <- v8()
ctx$source(file.path(pkg_js, "plugins/seedrandom.min.js"))
ctx$source(file.path(pkg_js, "d3.v4.min.js"))
ctx$source(file.path(pkg_js, "plugins/d3-weighted-voronoi.js"))
ctx$source(file.path(pkg_js, "plugins/d3-voronoi-map.js"))
ctx$source(file.path(pkg_js, "plugins/d3-voronoi-treemap.js"))

message("JS libraries loaded. Computing layout...")

ctx$eval(sprintf("var treeData = JSON.parse('%s');",
                 gsub("'", "\\\\'", n_json)))

ctx$eval("
  Math.seedrandom(42);
  var radius = 500;
  var points = 60;
  var inc = 2 * Math.PI / points;
  var clip = [];
  for (var i = 0; i < points; i++) {
    clip.push([radius + radius * Math.cos(i * inc),
               radius + radius * Math.sin(i * inc)]);
  }
  var hierarchy = d3.hierarchy(treeData).sum(function(d){ return d.weight || 0; });
  d3.voronoiTreemap().clip(clip)(hierarchy);
")

message("Layout computed. Extracting polygons...")

polys_json <- ctx$get(
  "JSON.stringify(hierarchy.leaves().map(function(d) {
    return {
      name:   d.data.name,
      weight: d.data.weight,
      color:  d.data.color,
      poly:   d.polygon.map(function(p){ return {x: p[0], y: p[1]}; })
    };
  }))"
)

# ── 4. Parse polygons into ggplot-ready data frame ───────────────────────────
polys <- jsonlite::fromJSON(polys_json)

poly_df <- pmap_dfr(polys, function(name, weight, color, poly, ...) {
  tibble(
    name   = name,
    weight = weight,
    color  = color,
    x      = poly$x,
    y      = poly$y
  )
}) |>
  mutate(
    # flip y (SVG y goes down, ggplot y goes up)
    y = -y,
    primary_cat = names(cat_pal)[match(color, cat_pal)]
  )

# Centroids for labels — show top games by ratings, scale text size to cell weight
centroids <- poly_df |>
  group_by(name, weight, color, primary_cat) |>
  summarise(cx = mean(x), cy = mean(y), .groups = "drop") |>
  mutate(
    label     = if_else(weight >= 15000, name, NA_character_),
    txt_size  = scales::rescale(sqrt(weight), to = c(3.2, 6.0),
                                from = sqrt(range(weight[weight >= 15000])))
  )

labeled <- filter(centroids, !is.na(label))

# ── 5. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot() +
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y, group = name, fill = primary_cat),
    colour = "white", linewidth = 0.3
  ) +
  # size is mapped per-row via a loop; use geom_text with aes size trick
  geom_text(
    data = labeled,
    aes(x = cx, y = cy, label = str_wrap(label, 11), size = txt_size),
    colour = "white", family = "sourcesans", fontface = "bold",
    lineheight = 0.41, show.legend = FALSE
  ) +
  scale_size_identity() +
  scale_fill_manual(values = cat_pal, name = NULL) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 5))) +
  coord_equal(clip = "off", expand = TRUE) +
  labs(
    title    = "A World of Board Games",
    subtitle = paste0(
      "The 200 most-rated games on BoardGameGeek, sized by number of ratings  •  ",
      comma(sum(games$usersrated)), " ratings in total"
    ),
    caption  = paste0(
      "Source: BoardGameGeek  |  ",
      "30 Day Chart Challenge – Day 24: South China Morning Post"
    )
  ) +
  theme_void(base_family = "sourcesans") +
  theme(
    plot.background  = element_rect(fill = "#111111", colour = NA),
    plot.title       = element_text(family = "playfair", face = "bold",
                                    size = 72, colour = "white",
                                    hjust = 0.5, margin = margin(t = 16, b = 6)),
    plot.subtitle    = element_text(size = 26, colour = "#AAAAAA",
                                    hjust = 0.5, margin = margin(b = 12)),
    plot.caption     = element_text(size = 36, colour = "#666666",
                                    hjust = 1, margin = margin(t = 10, b = 8)),
    plot.margin      = margin(8, 16, 8, 16),
    legend.position  = "bottom",
    legend.justification = "center",
    legend.text      = element_text(colour = "#CCCCCC", size = 22),
    legend.key.size  = unit(0.65, "cm"),
    legend.key.spacing.x = unit(0.4, "cm"),
    legend.key.spacing.y = unit(0.2, "cm"),
    legend.margin    = margin(t = 6, b = 4)
  )

ggsave("images/day24_scmp.png", p, width = 13, height = 13, dpi = 300)
