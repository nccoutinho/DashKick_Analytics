library(dplyr)
library(caret)

setwd("/Users/nats/DashKick_Analytics")

# Load your dataset
match_data <- read.csv("match_data.csv")
match_data


predict_win <- function(training_data, new_data, home_score, away_score, home_team, away_team) {
  # Create a binary outcome variable (1 if home team wins, 0 otherwise)
  training_data$Outcome <- ifelse(training_data[[home_score]] > training_data[[away_score]], 1, 0)
  
  # Train logistic regression model
  model_formula <- as.formula("Outcome ~ HomeTeam + AwayTeam + FT_ScoreHome + FT_scoreAway")
  model <- glm(model_formula, data = training_data, family = binomial)
  
  
