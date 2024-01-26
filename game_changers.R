library(httr)
library(tidyjson)
library(dplyr)
library(jsonlite)

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
    YellowCards = sapply(player_stats$statistics, function(stat) sum(stat$cards$yellow, na.rm = TRUE)),
    RedCards = sapply(player_stats$statistics, function(stat) sum(stat$cards$red, na.rm = TRUE))
  )
  
  all_players <- rbind(rbind(all_players, df)
  )
}


all_players



all_players$GoalsAssists = all_players$Goals + all_players$Assists


df_ordered <- all_players[order(all_players$GoalsAssists, decreasing = TRUE), ]

library(DT)
library(shiny)


# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Top 10 Premier League Goal Scorers"),
  DTOutput("goal_scorers_table")
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  # Render the interactive table with improved styling
  output$goal_scorers_table <- renderDT({
    datatable(
      df_ordered,
      options = list(
        paging = TRUE,
        lengthMenu = c(5, 10, 15),
        pageLength = 10,
        autoWidth = TRUE,
        responsive = TRUE
      )
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)