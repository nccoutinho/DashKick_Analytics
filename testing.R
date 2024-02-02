library(httr)

url <- "https://api-football-v1.p.rapidapi.com/v3/players"

queryString <- list(
  league = "39",
  season = "2023"
)

response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")


library(dashkickAnalytics)

match_result <- predict_win()