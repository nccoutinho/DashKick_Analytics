# Install and load ggplot2 package
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Install and load plotly package
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Function to track team performance
track_performance <- function(team_name) {
  url <- "https://api-football-v1.p.rapidapi.com/v3/fixtures"
  
  queryString <- list(
    league = "39",
    season = "2023"
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
  
  
  json_string <- content(response, "text")
  
  json_data <- fromJSON(json_string)
  
  match_stats <- json_data$response
  
  head(match_stats, 10)
  
  fixtures <- data.frame(
    FixtureDate = match_stats$fixture$date,
    TimeStamp = match_stats$fixture$timestamp,
    Stadium = match_stats$fixture$venue$name,
    Status = match_stats$fixture$status$short,
    HomeTeam = match_stats$teams$home$name,
    AwayTeam = match_stats$teams$away$name,
    Home_Winner = match_stats$teams$home$winner,
    FT_ScoreHome = match_stats$score$fulltime$home,
    FT_scoreAway = match_stats$score$fulltime$away,
    HT_ScoreHome = match_stats$score$halftime$home,
    HT_scoreAway = match_stats$score$halftime$away,
    Referee = match_stats$fixture$referee
  )
  
  # Filter data for the specified team
  team_data <- fixtures[fixtures$HomeTeam == team_name | fixtures$AwayTeam == team_name, ]
  
  # Create a new column to identify if the team is playing at home or away
  team_data$Location <- ifelse(team_data$HomeTeam == team_name, "Home", "Away")
  
  # Create a new variable to represent the order of fixtures
  team_data$FixtureNumber <- seq_len(nrow(team_data))
  
  # Filter data to include only rows with non-NA values in both 'FT_ScoreHome' and 'FT_scoreAway'
  team_data <- team_data[complete.cases(team_data[, c('FT_ScoreHome', 'FT_scoreAway')]), ]
  
  # Plotting
  plot <- ggplot(team_data, aes(x = FixtureNumber, 
                                y = ifelse(Location == "Home", FT_ScoreHome, FT_scoreAway),
                                color = Location,
                                text = paste("Opponent: ", ifelse(Location == "Home", AwayTeam, HomeTeam),
                                             "<br>Date: ", substr(FixtureDate, 1, 10),
                                             "<br>Stadium: ", Stadium,
                                             "<br>Referee: ", Referee))) +
    geom_line(aes(group = Location), linewidth = 1) +
    geom_point(aes(shape = Location), size = 2) +
    labs(title = paste("Team Performance: ", team_name),
         x = "Fixture Number",
         y = "Goals Scored",
         color = "Location",
         shape = "Location") +
    theme_minimal() +
    guides(shape = 'none')
  
  # Convert ggplot object to plotly object for interactive features
  plotly::ggplotly(plot, tooltip = "text")
}

# Example usage:
track_performance("Arsenal")