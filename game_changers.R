library(httr)
library(tidyjson)
library(dplyr)
library(jsonlite)
library(DT)
library(plotly)

url <- "https://api-football-v1.p.rapidapi.com/v3/players"

all_players <- list()

for(page in 1:47)
{
  queryString <- list(
    league = "39",
    season = "2023",
    page = page
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
  
  json_string <- content(response, "text")
  
  json_data <- fromJSON(json_string)
  
  player_stats <- json_data$response
  
  df <- data.frame(
    PlayerName = paste(player_stats$player$firstname, player_stats$player$lastname),
    PlayerAge = player_stats$player$age,
    Appearances = sapply(player_stats$statistics, function(stat) sum(stat$games$appearences, na.rm = TRUE)),
    Goals = sapply(player_stats$statistics, function(stat) sum(stat$goals$total, na.rm = TRUE)),
    Assists = sapply(player_stats$statistics, function(stat) sum(stat$goals$assists, na.rm = TRUE)),
    PenaltyGoals = sapply(player_stats$statistics, function(stat) sum(stat$penalty$scored, na.rm = TRUE)),
    YellowCards = sapply(player_stats$statistics, function(stat) sum(stat$cards$yellow, na.rm = TRUE)),
    RedCards = sapply(player_stats$statistics, function(stat) sum(stat$cards$red, na.rm = TRUE))
  )
  
  all_players <- rbind(rbind(all_players, df)
  )
}


all_players



all_players$GoalsAssists = all_players$Goals + all_players$Assists
all_players$NPG = all_players$Goals - all_players$PenaltyGoals

df_ordered <- all_players[order(all_players$Assists, decreasing = TRUE), ]

df <- df_ordered[1:20, ]



# Create a bar chart for goals and assists
plot_ly(df, x = ~NPG, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'NPG', marker = list(color = 'mediumpurple1')) %>%
  add_trace(x = ~PenaltyGoals, name = 'Penalty Goals', marker = list(color = 'orangered')) %>%
  add_trace(x = ~Assists, name = 'Assists', marker = list(color = 'lightblue1')) %>%
  layout(title = 'Top Players - Goals and Assists',
         xaxis = list(title = list(text = 'Count', font = list(family = 'Arial', size = 20), standoff = 20)),
         yaxis = list(title = list(text = 'Players', position = 'top', font = list(family = 'Arial', size = 20), standoff = 5, tickangle = -45)),
         barmode = 'stack') 
