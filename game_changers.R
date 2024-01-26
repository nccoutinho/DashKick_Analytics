library(DT)
library(shiny)

# Sample data for 10 players
top_scorers <- data.frame(
  Rank = 1:10,
  Player = c("Harry Kane", "Mohamed Salah", "Bruno Fernandes", "Patrick Bamford", "Son Heung-Min", "Dominic Calvert-Lewin", "Ollie Watkins", "Jamie Vardy", "Callum Wilson", "Wilfried Zaha"),
  Club = c("Tottenham Hotspur", "Liverpool", "Manchester United", "Leeds United", "Tottenham Hotspur", "Everton", "Aston Villa", "Leicester City", "Newcastle United", "Crystal Palace"),
  Goals = c(19, 17, 16, 15, 14, 14, 13, 12, 10, 9)
)

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
      top_scorers,
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
