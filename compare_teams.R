# Install and load httr package
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
library(httr)

# Install and load jsonlite package
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)

# Install and load ggradar package
if (!requireNamespace("ggradar", quietly = TRUE)) {
  install.packages("ggradar")
}
library(ggradar)

# Install and load tidyr package
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

# Install and load ggplot2 package
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

compare_teams <- function(team1_name, team2_name) {
  # Dictionary of team names and their IDs
  team_id_dict <- list(
    "Manchester United" = 33,
    "Aston Villa" = 66,
    "Bournemouth" = 35,
    "Fulham" = 36,
    "Newcastle" = 37,
    "Brentford" = 55,
    "Wolves" = 39,
    "Liverpool" = 40,
    "Nottingham Forest" = 65,
    "Arsenal" = 42,
    "Burnley" = 44,
    "Everton" = 45,
    "Tottenham" = 47,
    "West Ham" = 48,
    "Chelsea" = 49,
    "Manchester City" = 50,
    "Brighton" = 51,
    "Crystal Palace" = 52,
    "Luton" = 1359,
    "Sheffield Utd" = 62
  )
  
  # Function to get team ID from the dictionary
  get_team_id <- function(team_name) {
    return(team_id_dict[[team_name]])
  }
  
  # Get team IDs
  team1_id <- get_team_id(team1_name)
  team2_id <- get_team_id(team2_name)
  
  # Function to get team statistics based on team ID
  get_team_stats <- function(team_id) {
    url <- "https://api-football-v1.p.rapidapi.com/v3/teams/statistics"
    query <- list(league = "39", season = "2023", team = team_id)
    response <- VERB("GET", url, query = query, 
                     add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 
                                 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'),
                     content_type("application/octet-stream"))
    
    # Convert response to JSON
    json_string <- content(response, "text")
    json_data <- fromJSON(json_string)
    team_stats <- json_data$response
    
    return(team_stats)
  }
  
  # Get team statistics for both teams
  team1_stats <- get_team_stats(team1_id)
  team2_stats <- get_team_stats(team2_id)
  
  # Function to create a data frame from team statistics
  create_team_df <- function(team_stats) {
    team_df <- data.frame(
      TeamName = team_stats$team$name,
      TeamLogo = team_stats$team$logo,
      FixturesPlayedHome = as.integer(team_stats$fixtures$played$home),
      FixturesPlayedAway = as.integer(team_stats$fixtures$played$away),
      FixturesPlayedTotal = as.integer(team_stats$fixtures$played$total),
      FixturesWinsHome = as.integer(team_stats$fixtures$wins$home),
      FixturesWinsAway = as.integer(team_stats$fixtures$wins$away),
      FixturesWinsTotal = as.integer(team_stats$fixtures$wins$total),
      FixturesDrawsHome = as.integer(team_stats$fixtures$draws$home),
      FixturesDrawsAway = as.integer(team_stats$fixtures$draws$away),
      FixturesDrawsTotal = as.integer(team_stats$fixtures$draws$total),
      FixtureLosesHome = as.integer(team_stats$fixtures$loses$home),
      FixtureLosesAway = as.integer(team_stats$fixtures$loses$away),
      FixtureLosesTotal = as.integer(team_stats$fixtures$loses$total),
      GoalsForAvgHome = as.numeric(team_stats$goals$`for`$average$home),
      GoalsForAvgAway = as.numeric(team_stats$goals$`for`$average$away),
      GoalsForAvgTotal = as.numeric(team_stats$goals$`for`$average$total),
      GoalsAgainstAvgHome = as.numeric(team_stats$goals$against$average$home),
      GoalsAgainstAvgAway = as.numeric(team_stats$goals$against$average$away),
      GoalsAgainstAvgTotal = as.numeric(team_stats$goals$against$average$total),
      FailedToScoreHome = as.integer(team_stats$failed_to_score$home),
      FailedToScoreAway = as.integer(team_stats$failed_to_score$away),
      FailedToScoreTotal = as.integer(team_stats$failed_to_score$total),
      PenaltyScoredPercent = as.numeric(sub("%", "", team_stats$penalty$scored$percentage)),
      PenaltyMissedPercent = as.numeric(sub("%", "", team_stats$penalty$missed$percentage))
    )
    
    return(team_df)
  }
  
  # Create data frames for both teams
  team1_df <- create_team_df(team1_stats)
  team2_df <- create_team_df(team2_stats)
  
  metrics <- c("FixturesWinsHome", "FixturesWinsAway", 
               "FixturesDrawsHome", "FixturesDrawsAway", 
               "FixtureLosesHome", "FixtureLosesAway", 
               "GoalsForAvgHome", "GoalsForAvgAway", 
               "GoalsAgainstAvgHome", "GoalsAgainstAvgAway", 
               "FailedToScoreHome", "FailedToScoreAway")
  
  total_columns <- c("FixturesWinsTotal", "FixturesWinsTotal", 
                     "FixturesDrawsTotal", "FixturesDrawsTotal", 
                     "FixtureLosesTotal", "FixtureLosesTotal", 
                     "GoalsForAvgTotal", "GoalsForAvgTotal", 
                     "GoalsAgainstAvgTotal", "GoalsAgainstAvgTotal", 
                     "FailedToScoreTotal", "FailedToScoreTotal")
  
  # Subset the data frame for the required metrics and total columns (Arsenal)
  team1_metrics <- team1_df[, metrics]
  team1_totals <- team1_df[, total_columns]
  
  # Subset the data frame for the required metrics and total columns (Aston Villa)
  team2_metrics <- team2_df[, metrics]
  team2_totals <- team2_df[, total_columns]
  
  # Calculate percentages based on the total columns (Team 1)
  team1_metrics_percent <- as.data.frame(apply(team1_metrics, 1, function(row) row / team1_totals))
  
  # Calculate percentages based on the total columns (Team 2)
  team2_metrics_percent <- as.data.frame(apply(team2_metrics, 1, function(row) row / team2_totals))
  
  # Transpose the data for plotting (Team 1)
  radar_data_1 <- t(team1_metrics_percent)
  
  # Shorter variable names for plotting
  metric_names <- c("WinsHome",
                    "WinsAway", "DrawsHome", "DrawsAway",
                    "LosesHome", "LosesAway", "GoalsForHome",
                    "GoalsForAway", "GoalsAgainstHome", "GoalsAgainstAway",
                    "NoGoalsHome", "NoGoalsAway")
  
  # Create a data frame for plotting (Team 1)
  radar_df_1 <- data.frame(
    metric = rep(metric_names, ncol(radar_data_1)),
    value = as.vector(radar_data_1)
  )
  
  # Normalize the data to be between 0 and 1 (Team 1)
  normalized_radar_df_1 <- data.frame(
    metric = radar_df_1$metric,
    value = (radar_df_1$value - min(radar_df_1$value, na.rm = TRUE)) / 
      (max(radar_df_1$value, na.rm = TRUE) - min(radar_df_1$value, na.rm = TRUE))
  )
  
  # Transpose the data for plotting (Team 2)
  radar_data_2 <- t(team2_metrics_percent)
  
  # Create a data frame for plotting (Team 2)
  radar_df_2 <- data.frame(
    metric = rep(metric_names, ncol(radar_data_2)),
    value = as.vector(radar_data_2)
  )
  
  # Normalize the data to be between 0 and 1 (Team 2)
  normalized_radar_df_2 <- data.frame(
    metric = radar_df_2$metric,
    value = (radar_df_2$value - min(radar_df_2$value, na.rm = TRUE)) / 
      (max(radar_df_2$value, na.rm = TRUE) - min(radar_df_2$value, na.rm = TRUE))
  )
  
  # Spread the data
  new_df_1 <- spread(normalized_radar_df_1, key = metric, value = value)
  new_df_2 <- spread(normalized_radar_df_2, key = metric, value = value)
  combined_df <- rbind(new_df_1, new_df_2)
  
  # Add the "Team" column with values
  combined_df$Team <- c(team1_name, team2_name)
  
  # Reorder the columns to have "Team" as the first column
  combined_df <- combined_df[, c("Team", names(combined_df)[-ncol(combined_df)])]
  
  # Create the spider plot
  spider_plot <- ggradar(combined_df,
                         group.line.width = 1, 
                         group.point.size = 3,
                         group.colours = c("#00AFBB", "#FC4E07"),
                         axis.label.size = 2.5, grid.label.size = 4, 
                         legend.text.size = 10,
                         legend.position = "bottom") +
    ggtitle("Comparison of Team Performance") + theme(plot.title = element_text(size = 13))
  
  # Print the spider plot
  print(spider_plot)
                             
}

# Example usage:
compare_teams("Arsenal", "Manchester United")

library(testthat)

test_that("compare_teams function returns a ggradar object with correct attributes", {
  # Use example teams
  team1_name <- "Arsenal"
  team2_name <- "Manchester United"
  
  # Run the function
  compare_teams_result <- compare_teams(team1_name, team2_name)
  
  # Check if the result is a ggplot object
  expect_true(inherits(compare_teams_result, "gg"),
              "Result should be a ggplot object")
  
  # Check if the title is present
  expect_true("title" %in% names(compare_teams_result$labels),
              "Title should be present in ggplot object")
})
