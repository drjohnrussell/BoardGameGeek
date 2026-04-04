library(httr2)
library(xml2)
library(dplyr)


# 1. Define the base request
base_url <- "https://boardgamegeek.com/xmlapi2"
request_object <- request(base_url)
## get oauth token from .Renviron
oauth_token <- Sys.getenv("BGG_oauth_token")

# 2. Build the specific request for a game (e.g., Ra, ID 30549)
# Use req_url_path() and req_url_query()
# add a user agent with req_user_agent()
game_request <- request(base_url) |>
  req_url_path_append("thing") |>
  req_url_query(
    id = "30549",
    type = "boardgame",
    stats = 1
  ) |> 
  req_auth_bearer_token("5d4a30f5-58e7-446c-a9df-e16ca14503cd")

# 4. Perform the request and get the response
response <- req_perform(game_request)

# 5. Process the response (BGG returns XML)
game_xml <- response |> 
  resp_body_xml()
# Convert the XML content to a data frame
game_data <- as_list(game_xml)
