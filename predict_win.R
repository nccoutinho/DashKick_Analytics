library(dplyr)
library(tidyr)
library(caret)
library(nnet)
library(Metrics)
library(knitr)
library(kableExtra)

setwd("/Users/nats/DashKick_Analytics")

predict_win <- function() {
  soccer_data <- read.csv("match_data.csv")
  soccer_data$Time <- as.POSIXct(soccer_data$Time, format = "%H:%M:%S")
  soccer_data$GoalDiff <- soccer_data$FT_ScoreHome - soccer_data$FT_scoreAway
  
  soccer <- read.csv("stand_data.csv")
  soccer_data <- soccer_data %>%
    left_join(soccer %>% select(team, HP), by = c("HomeTeam" = "team"))
  
  soccer_data <- soccer_data %>%
    left_join(soccer %>% select(team, AP), by = c("AwayTeam" = "team"))
  
  soccer_data <- soccer_data %>%
    left_join(soccer %>% select(team, points), by = c("HomeTeam" = "team"))
  
  soccer_data <- soccer_data %>%
    left_join(soccer %>% select(team, points), by = c("AwayTeam" = "team"))
  
  
  soccer_data <- soccer_data %>%
    filter(Status == "FT")
  
  soccer_data$Stadium <- as.factor(soccer_data$Stadium)
  soccer_data$HomeTeam <- as.factor(soccer_data$HomeTeam)
  soccer_data$AwayTeam <- as.factor(soccer_data$AwayTeam)
  soccer_data$Referee <- as.factor(soccer_data$Referee)
  
  model_data <- soccer_data %>%
    select(HomeTeam, AwayTeam, Stadium, Time, GoalDiff, points.x, points.y, HP, AP)
  
  set.seed(123)  
  train_index <- createDataPartition(model_data$GoalDiff, p = 0.8, list = FALSE)
  train_data <- model_data[train_index, ]
  test_data <- model_data[-train_index, ]
  
  lm_model <- lm(GoalDiff ~ ., data = train_data)
  
  predictions_lm <- predict(lm_model, newdata = test_data)
  
  predictions_lm <- round(predictions_lm)
  
  threshold <- 0
  
  test_data$Outcome <- ifelse(predictions_lm > threshold, "Win",
                              ifelse(predictions_lm < threshold, "Loss", "Draw"))
  
  test_data$ActualOutcome <- ifelse(test_data$GoalDiff > threshold, "Win",
                                    ifelse(test_data$GoalDiff < threshold, "Loss", "Draw"))
  
  
  correct_predictions <- test_data$Outcome == test_data$ActualOutcome
  accuracy <- mean(correct_predictions)
  mse_lm <- mean((predictions_lm - test_data$GoalDiff)^2)
  rmse_lm <- sqrt(mse_lm)
  mae_lm <- mean(abs(predictions_lm - test_data$GoalDiff))
  
  
  cat("Regression Metrics for Linear Regression model: ")
  cat("\nAccuracy:", accuracy, "\n")
  cat("Mean Squared Error (MSE):", mse_lm, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse_lm, "\n")
  cat("Mean Absolute Error (MAE):", mae_lm, "\n")
  
  return(test_data)
}

match_outcomes <- predict_win()

predict_final_standings <- function(match_result) {
  url <- "https://api-football-v1.p.rapidapi.com/v3/standings"
  
  queryString <- list(
    season = "2023",
    league = "39")
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
  json_string <- content(response, "text")
  standings <- json_data$response
  
  stand_df <- data.frame(
    rank = standings$league$standings[[1]][[1]]$rank,
    team = standings$league$standings[[1]][[1]]$team$name,
    points = standings$league$standings[[1]][[1]]$points,
    MP = standings$league$standings[[1]][[1]]$all$played,
    Wins = standings$league$standings[[1]][[1]]$all$win,
    HomePlayed = standings$league$standings[[1]][[1]]$home$played,
    HomeWin = standings$league$standings[[1]][[1]]$home$win,
    AwayPlayed = standings$league$standings[[1]][[1]]$away$played,
    AwayWin = standings$league$standings[[1]][[1]]$away$win
  )
  
  home_standings <- match_result %>%
    group_by(Team = HomeTeam) %>%
    summarize(
      Points_Home = sum(case_when(
        GoalDiff > 0 ~ 3,
        GoalDiff == 0 ~ 1,
        TRUE ~ 0
      )),
      GoalDifference_Home = sum(GoalDiff)
    ) %>%
    arrange(desc(Points_Home), desc(GoalDifference_Home)) %>%
    mutate(Position_Home = row_number())
  
  away_standings <- match_result %>%
    group_by(Team = AwayTeam) %>%
    summarize(
      Points_Away = sum(case_when(
        GoalDiff < 0 ~ 3,
        GoalDiff == 0 ~ 1,
        TRUE ~ 0
      )),
      GoalDifference_Away = sum(-GoalDiff)  
    ) %>%
    arrange(desc(Points_Away), desc(GoalDifference_Away)) %>%
    mutate(Position_Away = row_number())
  
  combined_standings <- bind_rows(home_standings, away_standings) %>%
    group_by(Team) %>%
    summarize(
      Points = sum(coalesce(Points_Home, 0), coalesce(Points_Away, 0)),
      Goal_Difference = sum(coalesce(GoalDifference_Home, 0), coalesce(GoalDifference_Away, 0))
    ) %>%
    arrange(desc(Points), desc(Goal_Difference)) %>%
    mutate(Position = row_number())
  combined_standings <- combined_standings[, c("Position", setdiff(names(combined_standings), "Position"))]
  
  combined_standings <- combined_standings %>%
    left_join(stand_df %>% select(team, rank), by = c("Team" = "team"))
  
  combined_standings$Position_Displacement = combined_standings$rank - combined_standings$Position
  combined_standings <- combined_standings %>% select(-rank)
  
  df_colors <- transform(combined_standings, Color = ifelse(Position %in% c(1:4), "seagreen", ifelse(Position == 5, "#ccffcc", ifelse(Position %in% c(18:20), "firebrick1", "white"))))
  
  league_table <- kable(df_colors[, -ncol(df_colors)], "html") %>%
    kable_styling("striped", full_width = FALSE) %>%
    row_spec(which(df_colors$Color == "seagreen"), background = "seagreen") %>%
    row_spec(which(df_colors$Color == "#ccffcc"), background = "#ccffcc") %>%
    row_spec(which(df_colors$Color == "firebrick1"), background = "firebrick1") %>%
    row_spec(1, extra_css = "font-weight:bold;")
  
  print(league_table)
  
  return(combined_standings)
}

final_stand <- predict_final_standings(match_outcomes)



