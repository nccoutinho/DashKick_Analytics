library(httr)
library(tidyjson)
library(dplyr)
library(jsonlite)
library(DT)
library(plotly)

game_changers <- function() {
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
      Team = sapply(player_stats$statistics, function(stat) (stat$team$name[[1]])),
      PlayerAge = player_stats$player$age,
      Appearances = sapply(player_stats$statistics, function(stat) sum(stat$games$appearences, na.rm = TRUE)),
      Goals = sapply(player_stats$statistics, function(stat) sum(stat$goals$total, na.rm = TRUE)),
      Assists = sapply(player_stats$statistics, function(stat) sum(stat$goals$assists, na.rm = TRUE)),
      PenaltyGoals = sapply(player_stats$statistics, function(stat) sum(stat$penalty$scored, na.rm = TRUE))
    )
    
    all_players <- rbind(rbind(all_players, df)
    )
  }
  
  all_players$GoalsAssists = all_players$Goals + all_players$Assists
  df_ordered <- all_players[order(all_players$GoalsAssists, decreasing = TRUE), ]
  
  df_unique <- df_ordered %>%
    distinct(PlayerName, .keep_all = TRUE)
  
  df_20 <- df_unique[1:20, ]
  
  show_bar <- top_20(df_20)
  
  print(show_bar)
  
  return (df_20)
}


top_20 <- function(df) {
  show_bar <- plot_ly(df, x = ~Goals, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Goals', marker = list(color = 'mediumpurple1'),
          hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Goals: %{x}',
          customdata = ~Team) %>%
    add_trace(x = ~Assists, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Assists', marker = list(color = 'lightblue1'),
              hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Assists: %{x}',
              customdata = ~Team) %>%
    layout(title = 'Top Players - Goals and Assists',
           xaxis = list(title = list(text = 'Count', font = list(family = 'Arial', size = 20), standoff = 20)),
           yaxis = list(title = list(text = 'Players', position = 'top', font = list(family = 'Arial', size = 20), standoff = 5, tickangle = -45)),
           barmode = 'stack')
  
  return (show_bar)
}


df_20 <- game_changers()

df_unique <- df_20 %>%
  distinct(PlayerName, .keep_all = TRUE)

show_bar <- top_20(df_unique)

print(show_bar)