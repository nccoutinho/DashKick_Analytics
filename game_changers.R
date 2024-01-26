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

df <- df_ordered[1:20, ]
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
      df,
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


library(kableExtra)
library(dplyr)


# Create a pretty tabular format
table <- df %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Display the table
table






# Load required libraries
library(knitr)
library(kableExtra)

# Your data
data <- data.frame(
  PlayerName = c("Mohamed Salah Hamed Mahrous Ghaly", "Erling Braut Haaland", "Cole Jermaine Palmer", "Heung-Min Son", 
                 "Oliver George Arthur Watkins", "Hee-Chan Hwang", "Jarrod Bowen", "Anthony Michael Gordon", 
                 "Bukayo Ayoyinka Temidayo Saka", "Leon Patrick Bailey Butler", "Julián Álvarez", "Philip Walter Foden", 
                 "Darwin Gabriel Núñez Ribeiro", "Alexander Isak", "Richarlison de Andrade", 
                 "Bernardo Mota Veiga de Carvalho e Silva", "Bryan Tetsadong Marceau Mbeumo", "Nicolas Jackson", 
                 "João Pedro Junqueira de Jesus", "Pascal Groß"),
  PlayerAge = c(32, 24, 22, 32, 29, 28, 28, 23, 23, 27, 24, 24, 25, 25, 27, 30, 25, 23, 23, 33),
  Appearances = c(20, 15, 18, 20, 21, 20, 19, 20, 19, 19, 20, 20, 19, 16, 18, 17, 15, 19, 20, 18),
  Goals = c(14, 14, 9, 12, 9, 10, 11, 7, 6, 6, 6, 5, 5, 10, 7, 6, 7, 7, 7, 3),
  Assists = c(8, 4, 8, 5, 8, 3, 2, 5, 6, 5, 5, 6, 6, 0, 3, 4, 3, 2, 2, 6),
  YellowCards = c(2, 1, 6, 1, 4, 4, 1, 7, 2, 3, 2, 1, 4, 0, 3, 4, 1, 8, 2, 4),
  RedCards = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  GoalsAssists = c(22, 18, 17, 17, 17, 13, 13, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9),
  ImageURL = c(
    "https://example.com/image1.png",
    "https://example.com/image2.png",
    "https://example.com/image3.png",
    "https://example.com/image4.png",
    "https://example.com/image5.png",
    "https://example.com/image6.png",
    "https://example.com/image7.png",
    "https://example.com/image8.png",
    "https://example.com/image9.png",
    "https://example.com/image10.png",
    "https://example.com/image11.png",
    "https://example.com/image12.png",
    "https://example.com/image13.png",
    "https://example.com/image14.png",
    "https://example.com/image15.png",
    "https://example.com/image16.png",
    "https://example.com/image17.png",
    "https://example.com/image18.png",
    "https://example.com/image19.png",
    "https://example.com/image20.png"
  )
)

# Create a ranked version of the data based on GoalsAssists
data_ranked <- data[order(-data$GoalsAssists), ]

# Create a color gradient for GoalsAssists column using RColorBrewer palette
color_scale <- RColorBrewer::brewer.pal(n = max(data$GoalsAssists) + 1, name = "YlGnBu")

# Assign colors based on rank
data_ranked$GoalsAssistsColor <- color_scale[rank(-data_ranked$GoalsAssists)]

# Display the ranked data as a colored table with images
kable(data_ranked, "html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  column_spec(1, image = spec(data_ranked$ImageURL, width = "50px", height = "50px")) %>%
  row_spec(row = 1, background = "#FFD700")  # Highlight the header row




# Load libraries
library(DT)
library(plotly)

# Create a data frame
data <- data.frame(
  PlayerName = c("Mohamed Salah", "Erling Haaland", "Cole Palmer", "Heung-Min Son", "Ollie Watkins", "Hee-Chan Hwang", "Jarrod Bowen", "Anthony Gordon", "Bukayo Saka", "Leon Bailey", "Julián Álvarez", "Phil Foden", "Darwin Núñez", "Alexander Isak", "Richarlison", "Bernardo Silva", "Bryan Mbeumo", "Nicolas Jackson", "João Pedro", "Pascal Groß"),
  PlayerAge = c(32, 24, 22, 32, 29, 28, 28, 23, 23, 27, 24, 24, 25, 25, 27, 30, 25, 23, 23, 33),
  Appearances = c(20, 15, 18, 20, 21, 20, 19, 20, 19, 19, 20, 20, 19, 16, 18, 17, 15, 19, 20, 20),
  Goals = c(14, 14, 9, 12, 9, 10, 11, 7, 6, 6, 6, 5, 5, 10, 7, 6, 7, 7, 7, 7),
  Assists = c(8, 4, 8, 5, 8, 3, 2, 5, 6, 5, 5, 6, 6, 0, 3, 4, 3, 2, 2, 2),
  YellowCards = c(2, 1, 6, 1, 4, 4, 1, 7, 2, 3, 2, 1, 4, 0, 3, 4, 1, 8, 2, 4),
  RedCards = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  GoalsAssists = c(22, 18, 17, 17, 17, 13, 13, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9)
)

# Create a DataTable
datatable(data, options = list(pageLength = 10))

# Create a bar chart for goals and assists
plot_ly(data, x = ~PlayerName, y = ~Goals, type = 'bar', name = 'Goals', marker = list(color = 'blue')) %>%
  add_trace(y = ~Assists, name = 'Assists', marker = list(color = 'green')) %>%
  layout(title = 'Top Players - Goals and Assists',
         xaxis = list(title = 'Players'),
         yaxis = list(title = 'Count'),
         barmode = 'stack')