# 30 Day Chart Challenge — Board Game Geek Edition

A series of data visualisations built in R as part of the [#30DayChartChallenge](https://github.com/30DayChartChallenge). Every chart uses data from [BoardGameGeek](https://boardgamegeek.com/) — either the static rankings dataset or the BGG XML API v2 — as an opportunity to learn the API and explore what the BGG community reveals about board game trends, mechanics, ratings, and more.

**Stack:** R · tidyverse · ggplot2 · httr2 · xml2 · gganimate · and various ggplot2 extension packages

---

## Charts

| Day | Theme | Chart | File |
|-----|-------|-------|------|
| 1 | Part-to-Whole | Treemap — mechanics share across the top 500 games | `day01_part_to_whole.R` |
| 2 | Pictogram | Icon grid — top 100 games by BGG sub-category | `day02_pictogram.R` |
| 3 | Mosaic | Mosaic chart — games by category and sub-category | `day03_mosaic.R` |
| 4 | Slope | Slope chart — raw vs Bayes-adjusted rating, top 300 games | `day04_slope.R` |
| 5 | Experimental | Nightingale rose — games per sub-category, top-500 and top-100 layers | `day05_rose.R` |
| 6 | Data Journalism | Reporters Without Borders Press Freedom Index | `day06_reporters.R` |
| 7 | Multiscale | Log-log scatter — sub-category rank vs overall BGG rank | `day07_multiscale.R` |
| 8 | Circular | Chord diagram — mechanic co-occurrence in the top 500 games | `day08_circular.R` |
| 9 | Wealth | Lorenz curve — inequality of BGG ratings across all games, with Gini coefficient | `day09_wealth.R` |
| 10 | Pop Culture | Bar chart — pop-culture franchises in the top 1,000 most-rated games | `day10_popculture.R` |
| 11 | Physical | Scatter — box footprint vs complexity weight, top 500 ranked games | `day11_physical.R` |
| 12 | FlowingData | Ridgeline plot — Bayes rating distributions by BGG sub-category | `day12_flowingdata.R` |
| 13 | Ecosystems | Alluvial/Sankey — top 500 games flowing from sub-category to complexity tier | `day13_ecosystems.R` |
| 14 | Trade | Network graph — designer collaboration network in the top 500 games | `day14_trade.R` |
| 15 | Correlation | Dual loess lines — raw vs Bayes-adjusted rating by publication year (1970–2024) | `day15_correlation.R` |
| 16 | Causation | Forest plot — mean Bayes rating ± 95% CI by mechanic, relative to overall mean | `day16_causation.R` |
| 17 | Remake | Gapminder-style bubble chart — year published vs Bayes rating, sized by ratings count | `day17_remake.R` |
| 18 | UNICEF | Dual choropleth — under-five mortality rate and estimate uncertainty by country, 2024 | `day18_unicef.R` |
| 19 | Evolution | Stacked area — share of BGG-ranked games by sub-category, 1980–2024 | `day19_evolution.R` |
| 20 | Global Change | Smoothed share lines — rise of Worker Placement & Deck Building, decline of Set Collection | `day20_globalchange.R` |
| 21 | Historical | Treemap — top-1,000 games tagged with History/Ancient families, coloured by era | `day21_historical.R` |
| 22 | New Tool | Violin + boxplot + jitter — Bayes rating distribution by sub-category, top 500 games | `day22_newtool.R` |
| 23 | Seasons | Polar bar chart — personal BGG play counts by month, coloured by season | `day23_seasons.R` |
| 24 | South China Morning Post | Voronoi treemap — top 200 most-rated games sized by ratings count, coloured by sub-category | `day24_scmp.R` |
| 25 | Space | Star-map scatter — space & sci-fi games, glow effect encoding rating certainty | `day25_space.R` |
| 26 | Trend | Smoothed line — share of top-1,000 games with a Solo/Solitaire mechanic, 1985–2024 | `day26_trend.R` |
| 27 | Animate | Bar chart race — most common mechanics in BGG's top 1,000 games, 1993–2024 | `day27_animate.R` |

---

## Data sources

- **`data/boardgames_ranks.csv`** — BGG rankings snapshot: one row per game with rank, ratings, sub-category ranks, and expansion flag
- **BGG XML API v2** (`https://boardgamegeek.com/xmlapi2`) — game details, mechanics, designers, families, play logs, and more
- Cached API pulls stored as `.rds` files: `mechanics_raw_1000.rds`, `physical_raw.rds`, `designers_raw.rds`, `families_raw.rds`, `plays_raw.rds`
