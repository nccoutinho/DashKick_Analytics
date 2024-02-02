#' Function 1 Title
#'
#' Description of what the function does.
#'
#' @return What the function returns.
#' @examples
#' compare_teams('Arsenal', 'Manchester United')
#'
#'
#' @import httr
#' @import tidyjson
#' @import dplyr
#' @import ggradar
#' @import ggplot2
#' @import tidyr
#' @import DT
#'
#' @export
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
    
    tryCatch(
      {
        response <- VERB("GET", url, query = query, 
                         add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 
                                     'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'),
                         content_type("application/octet-stream"))
        
        # Convert response to JSON
        json_string <- content(response, "text", encoding='UTF-8')
        json_data <- fromJSON(json_string)
        team_stats <- json_data$response
        
        return(team_stats)
      },
      error = function(e) {
        cat("Error in get_team_stats:", conditionMessage(e), "\n")
        # You can choose how to handle errors here, e.g., return a default or empty result
        return(NULL)
      }
    )
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



#' Function 2 Title
#'
#' Description of what the function does.
#'
#' @return What the function returns.
#' @examples
#' track_performance('Arsenal')
#'
#'
#' @import httr
#' @import tidyjson
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import tidyr
#' @import DT
#'
#' @export
track_performance <- function(team_name) {
  url <- "https://api-football-v1.p.rapidapi.com/v3/fixtures"
  
  queryString <- list(
    league = "39",
    season = "2023"
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
  
  
  json_string <- content(response, "text", encoding='UTF-8')
  
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





#' Top 20 Function (Internal)
#'
#' Description of what the function does. (This function is not intended for user access.)
#'
#' @return What the function returns.
#' @examples
#' # This function is not meant to be called directly by users.
#'
#' @import DT
#' @import httr
#' @import tidyjson
#' @import dplyr
#' @import jsonlite
#' @import plotly
#'
#'
#' @keywords internal
top_20 <- function(df) {
  show_bar <- plot_ly(df, x = ~Goals, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Goals', marker = list(color = 'mediumpurple1'),
                      hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Goals: %{x}',
                      customdata = ~Team) %>%
    add_trace(x = ~Assists, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Assists', marker = list(color = 'lightblue1'),
              hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Assists: %{x}',
              customdata = ~Team) %>%
    layout(title = 'Top Game Changers of the EPL 23-24',
           xaxis = list(title = list(text = 'Count', font = list(family = 'Arial', size = 20), standoff = 20)),
           yaxis = list(title = list(text = 'Players', position = 'top', font = list(family = 'Arial', size = 20), standoff = 5, tickangle = -45)),
           barmode = 'stack')
  
  return (show_bar)
}