library(dplyr)
library(caret)
library(nnet)

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
  standings <- match_result %>%
    group_by(Team = HomeTeam) %>%
    summarize(
      Points = sum(case_when(
        GoalDiff > 0 ~ 3,
        GoalDiff == 0 ~ 1,
        TRUE ~ 0
      )),
      GoalDifference = sum(GoalDiff)
    ) 
final_stand <- predict_final_standings(match_outcomes)



