library(httr2)
library(xml2)
library(dplyr)
library(purrr)

base_url  <- "https://boardgamegeek.com/xmlapi2"
bgg_token <- Sys.getenv("BGG_oauth_token")

# ── Helper ────────────────────────────────────────────────────────────────────
bgg_get <- function(...) {
  request(base_url) |>
    req_url_path_append("thing") |>
    req_url_query(...) |>
    req_user_agent("30DayChartChallenge BGG/1.0") |>
    req_auth_bearer_token(bgg_token) |>
    req_retry(max_tries = 3, backoff = \(x) 10) |>
    req_perform() |>
    resp_body_xml()
}

# ── 1. Fetch Gloomhaven (174430) with stats + versions ────────────────────────
xml  <- bgg_get(id = "174430", type = "boardgame", stats = 1, versions = 1)
item <- xml_find_first(xml, "//item")

cat("\n=== TOP-LEVEL ATTRIBUTES ===\n")
cat("type:", xml_attr(item, "type"), "\n")
cat("id:  ", xml_attr(item, "id"),   "\n")

cat("\n=== SCALAR FIELDS ===\n")
scalars <- c("yearpublished","minplayers","maxplayers",
             "minplaytime","maxplaytime","playingtime","minage")
for (s in scalars) {
  node <- xml_find_first(item, paste0(".//", s))
  cat(sprintf("  %-20s %s\n", s, xml_attr(node, "value")))
}
primary_name <- xml_find_first(item, ".//name[@type='primary']")
cat(sprintf("  %-20s %s\n", "name (primary)", xml_attr(primary_name, "value")))

cat("\n=== CHILD ELEMENT COUNTS ===\n")
print(sort(table(xml_name(xml_children(item))), decreasing = TRUE))

cat("\n=== ALL LINK TYPES ===\n")
links    <- xml_find_all(item, ".//link")
link_df  <- tibble(
  type  = xml_attr(links, "type"),
  id    = xml_attr(links, "id"),
  value = xml_attr(links, "value")
)
print(sort(table(link_df$type), decreasing = TRUE))

cat("\n=== LINK VALUES BY TYPE ===\n")
for (lt in sort(unique(link_df$type))) {
  vals <- link_df |> filter(type == lt) |> pull(value)
  cat(sprintf("\n[%s] (%d items)\n", lt, length(vals)))
  cat(paste(" -", head(vals, 8), collapse = "\n"), "\n")
  if (length(vals) > 8) cat(" ... and", length(vals) - 8, "more\n")
}

cat("\n=== POLLS ===\n")
polls <- xml_find_all(item, ".//poll")
for (p in polls) {
  cat(sprintf("\nPoll: %-35s  totalvotes=%s\n",
    xml_attr(p, "name"), xml_attr(p, "totalvotes")))
  results <- xml_find_all(p, ".//result")
  for (r in head(results, 4)) {
    attrs <- xml_attrs(r)
    cat("  ", paste(names(attrs), attrs, sep = "=", collapse = "  "), "\n")
  }
  if (length(results) > 4) cat("  ... and", length(results) - 4, "more results\n")
}

cat("\n=== RATINGS / STATS ===\n")
ratings <- xml_find_first(item, ".//ratings")
for (sc in xml_children(ratings)) {
  nm  <- xml_name(sc)
  val <- xml_attr(sc, "value")
  if (!is.na(val)) {
    cat(sprintf("  %-25s %s\n", nm, val))
  } else {
    cat(sprintf("  %s (nested):\n", nm))
    for (ss in xml_children(sc)) {
      a <- xml_attrs(ss)
      cat("    ", paste(names(a), a, sep = "=", collapse = "  "), "\n")
    }
  }
}

cat("\n=== VERSIONS (first version sample) ===\n")
ver <- xml_find_first(item, ".//versions/item")
if (!is.na(ver)) {
  cat("  Version attributes:", paste(names(xml_attrs(ver)), xml_attrs(ver), sep="=", collapse="  "), "\n")
  for (vc in xml_children(ver)) {
    nm  <- xml_name(vc)
    val <- xml_attr(vc, "value")
    if (!is.na(val)) cat(sprintf("  %-25s %s\n", nm, val))
  }
}

cat("\n=== COLLECTION ENDPOINT FIELDS (sample: own=1) ===\n")
col_xml <- request(base_url) |>
  req_url_path_append("collection") |>
  req_url_query(username = "Zefquaavius", subtype = "boardgame", own = 1, stats = 1, page = 1) |>
  req_user_agent("30DayChartChallenge BGG/1.0") |>
  req_auth_bearer_token(bgg_token) |>
  req_retry(max_tries = 3, backoff = \(x) 10) |>
  req_perform() |>
  resp_body_xml()

col_item <- xml_find_first(col_xml, "//item")
if (!is.na(col_item)) {
  cat("Collection item child elements:\n")
  print(table(xml_name(xml_children(col_item))))
  status <- xml_find_first(col_item, ".//status")
  if (!is.na(status)) {
    a <- xml_attrs(status)
    cat("Status attrs:", paste(names(a), a, sep="=", collapse="  "), "\n")
  }
}

cat("\nDone.\n")
