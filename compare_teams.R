library(httr)

url <- "https://api-football-v1.p.rapidapi.com/v3/teams/statistics"

queryString <- list(
  league = "39",
  season = "2023",
  team = "42"
)

response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")

json_string <- content(response, "text")

library(jsonlite)

json_data <- fromJSON(json_string)
arsenal_stats <- json_data$response
arsenal_stats

arsenal_df <- data.frame(
  TeamName = arsenal_stats$team$name,
  TeamLogo = arsenal_stats$team$logo,
  FixturesPlayedHome = as.integer(arsenal_stats$fixtures$played$home),
  FixturesPlayedAway = as.integer(arsenal_stats$fixtures$played$away),
  FixturesPlayedTotal = as.integer(arsenal_stats$fixtures$played$total),
  FixturesWinsHome = as.integer(arsenal_stats$fixtures$wins$home),
  FixturesWinsAway = as.integer(arsenal_stats$fixtures$wins$away),
  FixturesWinsTotal = as.integer(arsenal_stats$fixtures$wins$total),
  FixturesDrawsHome = as.integer(arsenal_stats$fixtures$draws$home),
  FixturesDrawsAway = as.integer(arsenal_stats$fixtures$draws$away),
  FixturesDrawsTotal = as.integer(arsenal_stats$fixtures$draws$total),
  FixtureLosesHome = as.integer(arsenal_stats$fixtures$loses$home),
  FixtureLosesAway = as.integer(arsenal_stats$fixtures$loses$away),
  FixtureLosesTotal = as.integer(arsenal_stats$fixtures$loses$total),
  GoalsForAvgHome = as.numeric(arsenal_stats$goals$`for`$average$home),
  GoalsForAvgAway = as.numeric(arsenal_stats$goals$`for`$average$away),
  GoalsForAvgTotal = as.numeric(arsenal_stats$goals$`for`$average$total),
  GoalsAgainstAvgHome = as.numeric(arsenal_stats$goals$against$average$home),
  GoalsAgainstAvgAway = as.numeric(arsenal_stats$goals$against$average$away),
  GoalsAgainstAvgTotal = as.numeric(arsenal_stats$goals$against$average$total),
  FailedToScoreHome = as.integer(arsenal_stats$failed_to_score$home),
  FailedToScoreAway = as.integer(arsenal_stats$failed_to_score$away),
  FailedToScoreTotal = as.integer(arsenal_stats$failed_to_score$total),
  PenaltyScoredPercent = as.numeric(sub("%", "", arsenal_stats$penalty$scored$percentage)),
  PenaltyMissedPercent = as.numeric(sub("%", "", arsenal_stats$penalty$missed$percentage))
)
arsenal_df


# Select the metrics and total columns
metrics <- c("FixturesPlayedHome", "FixturesPlayedAway", 
             "FixturesWinsHome", "FixturesWinsAway", 
             "FixturesDrawsHome", "FixturesDrawsAway", 
             "FixtureLosesHome", "FixtureLosesAway", 
             "GoalsForAvgHome", "GoalsForAvgAway", 
             "GoalsAgainstAvgHome", "GoalsAgainstAvgAway", 
             "FailedToScoreHome", "FailedToScoreAway")

total_columns <- c("FixturesPlayedTotal", "FixturesPlayedTotal", 
                   "FixturesWinsTotal", "FixturesWinsTotal", 
                   "FixturesDrawsTotal", "FixturesDrawsTotal", 
                   "FixtureLosesTotal", "FixtureLosesTotal", 
                   "GoalsForAvgTotal", "GoalsForAvgTotal", 
                   "GoalsAgainstAvgTotal", "GoalsAgainstAvgTotal", 
                   "FailedToScoreTotal", "FailedToScoreTotal")

# Subset the data frame for the required metrics and total columns
arsenal_metrics <- arsenal_df[, metrics]
arsenal_totals <- arsenal_df[, total_columns]

# Calculate percentages based on the total columns
arsenal_metrics_percent <- as.data.frame(apply(arsenal_metrics, 1, function(row) row / arsenal_totals))

# Manually assign new variable names
new_names <- c("FixPlayedHome_Percent", "FixPlayedAway_Percent", 
               "WinsHome_Percent", "WinsAway_Percent", 
               "DrawsHome_Percent", "DrawsAway_Percent", 
               "LosesHome_Percent", "LosesAway_Percent", 
               "GoalsForAvgHome_Percent", "GoalsForAvgAway_Percent", 
               "GoalsAgainstAvgHome_Percent", "GoalsAgainstAvgAway_Percent", 
               "FailedToScoreHome_Percent", "FailedToScoreAway_Percent")

# Assign new variable names to the columns
colnames(arsenal_metrics_percent) <- new_names

# Print the updated DataFrame
print(arsenal_metrics_percent)

# Load required libraries
library(ggradar)

# Assuming 'arsenal_metrics_percent' is the updated DataFrame

# Manually assign metric names for the radar chart
metric_names <- c("FixPlayedHome", "FixPlayedAway", 
                  "WinsHome", "WinsAway", 
                  "DrawsHome", "DrawsAway", 
                  "LosesHome", "LosesAway", 
                  "GoalsForAvgHome", "GoalsForAvgAway", 
                  "GoalsAgainstAvgHome", "GoalsAgainstAvgAway", 
                  "FailedToScoreHome", "FailedToScoreAway")

# Transpose the data for plotting
radar_data <- t(arsenal_metrics_percent)

# Create a data frame for plotting
radar_df <- data.frame(
  metric = rep(metric_names, ncol(radar_data)),
  value = as.vector(radar_data)
)

# Normalize the data to be between 0 and 1
normalized_radar_df <- data.frame(
  metric = radar_df$metric,
  value = (radar_df$value - min(radar_df$value, na.rm = TRUE)) / 
    (max(radar_df$value, na.rm = TRUE) - min(radar_df$value, na.rm = TRUE))
)

library(tidyr)

# Spread the data
new_df <- spread(normalized_radar_df, key = metric, value = value)

# Print the new data frame
print(new_df)

# Create the spider plot
spider_plot <- ggradar(new_df)

# Print the spider plot
print(spider_plot)












