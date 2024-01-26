# Function to track team performance
track_performance <- function(team_name) {
  # Load necessary libraries
  library(ggplot2)
  
  # Constant file path
  csv_file_path <- "/Users/christophermulya/Downloads/match_data.csv"  # Replace with the actual file path
  
  # Read the CSV file
  fixtures <- read.csv(csv_file_path)
  
  # Filter data for the specified team
  team_data <- fixtures[fixtures$HomeTeam == team_name | fixtures$AwayTeam == team_name, ]
  
  # Create a new column to identify if the team is playing at home or away
  team_data$Location <- ifelse(team_data$HomeTeam == team_name, "Home", "Away")
  
  # Plotting
  ggplot(team_data, aes(x = FixtureDate, y = FT_ScoreHome, color = Location)) +
    geom_line(aes(group = Location), linewidth = 1) +
    geom_point(aes(shape = Location), size = 2) +
    labs(title = paste("Team Performance: ", team_name),
         x = "Fixture Date",
         y = "Goals Scored",
         color = "Location",
         shape = "Location") +
    theme_minimal()
}

# Example usage:
track_performance("Arsenal")