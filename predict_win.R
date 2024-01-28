library(dplyr)
library(caret)
library(nnet)

setwd("/Users/nats/DashKick_Analytics")

# Load your dataset 
match_data <- read.csv("match_data.csv")

predict_win <- function() {
  match_data$Home_Winner_Binary <- ifelse(is.na(match_data$Home_Winner), "Draw", ifelse(match_data$Home_Winner, "Win", "Defeat"))
  
  #match_data <- match_data[1:200, ]
  # Create train and test data partitions
  set.seed(123)  # Set seed for reproducibility
  train_index <- createDataPartition(match_data$Home_Winner_Binary, p = 0.8, list = FALSE)
  train_data <- match_data[train_index, ]
  test_data <- match_data[-train_index, ]
  
  # Train logistic regression model
  model_formula <- as.formula("Home_Winner_Binary ~ Stadium + FT_ScoreHome + FT_scoreAway + HT_ScoreHome + HT_scoreAway")
  model <- multinom(model_formula, data = train_data)
  
  # Make predictions on new data
  predictions <- predict(model, newdata = test_data, type = "class")
  
  # Convert predicted probabilities to binary outcome
  levels_to_use <- c("Draw", "Win", "Defeat")
  test_data$Home_Winner_Binary <- factor(test_data$Home_Winner_Binary, levels = levels_to_use)
  predictions <- factor(predictions, levels = levels_to_use)
  test_data$PredictedOutcomes <- predictions
  
  confusion_matrix <- confusionMatrix(predictions, test_data$Home_Winner_Binary)
  print(confusion_matrix)
  
  # Extract values from confusion matrix
  accuracy <- confusion_matrix$overall[1]  # Access the 'Accuracy' element
  precision <- confusion_matrix$byClass[, "Precision"]
  recall <- confusion_matrix$byClass[, "Recall"]
  f1_score <- confusion_matrix$byClass[, "F1"]
  
  cat("\nAccuracy:", accuracy, "\n")
  cat("Precision:", mean(precision, na.rm = TRUE), "\n")  # Use mean to handle possible NA values
  cat("Recall:", mean(recall, na.rm = TRUE), "\n")  # Use mean to handle possible NA values
  cat("F1 Score:", mean(f1_score, na.rm = TRUE), "\n")  # Use mean to handle possible NA values
  
  return(test_data)
}

match_outcomes <- predict_win()





