#' Function 1 Title
#'
#' Description of what the function does.
#'
#' @return What the function returns.
#' @examples
#' game_changers()
#'
#'
#' @import httr
#' @import tidyjson
#' @import dplyr
#' @import caret
#' @import knitr
#' @import kableExtra
#' @import nnet
#' @import Metrics
#' @import DT
#'
#' @export
game_changers <- function() {
  url <- "https://api-football-v1.p.rapidapi.com/v3/players"
  all_players <- list()

  for(page in 1:47)
  {
    queryString <- list(
      league = "39",
      season = "2023",
      page = page
    )

    response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
    json_string <- content(response, "text", encoding='UTF-8')
    print(json_string)
    json_data <- fromJSON(json_string)
    player_stats <- json_data$response

    df <- data.frame(
      PlayerName = paste(player_stats$player$firstname, player_stats$player$lastname),
      Team = sapply(player_stats$statistics, function(stat) (stat$team$name[[1]])),
      PlayerAge = player_stats$player$age,
      Appearances = sapply(player_stats$statistics, function(stat) sum(stat$games$appearences, na.rm = TRUE)),
      Goals = sapply(player_stats$statistics, function(stat) sum(stat$goals$total, na.rm = TRUE)),
      Assists = sapply(player_stats$statistics, function(stat) sum(stat$goals$assists, na.rm = TRUE)),
      PenaltyGoals = sapply(player_stats$statistics, function(stat) sum(stat$penalty$scored, na.rm = TRUE))
    )

    all_players <- rbind(rbind(all_players, df)
    )
  }

  all_players$GoalsAssists = all_players$Goals + all_players$Assists
  df_ordered <- all_players[order(all_players$GoalsAssists, decreasing = TRUE), ]

  df_unique <- df_ordered %>%
    distinct(PlayerName, .keep_all = TRUE)

  suppressWarnings({
    df_20 <- df_unique[1:20, ]
    show_bar <- top_20(df_20)
    print(show_bar)
  })

  return (df_20)
}



#' Function 2 Title
#'
#' Description of what the function does.
#'
#' @return What the function returns.
#' @examples
#' predict_win()
#'
#'
#'
#' @import httr
#' @import jsonlite
#' @import DT
#' @import tidyr
#' @import caret
#' @importFrom caret createDataPartition
#' @import nnet
#' @import Metrics
#' @import knitr
#' @import kableExtra
#'
#'
#' @export
predict_win <- function() {
  url <- "https://api-football-v1.p.rapidapi.com/v3/fixtures"

  queryString <- list(
    league = "39",
    season = "2023"
  )

  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

  json_string <- content(response, "text")

  json_data <- fromJSON(json_string)

  json_data <- fromJSON(json_string)

  match_stats <- json_data$response



  #head(match_stats, 10)

  soccer_data <- data.frame(
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



  soccer_data$GoalDiff <- soccer_data$FT_ScoreHome - soccer_data$FT_scoreAway

  soccer_data$Stadium <- as.factor(soccer_data$Stadium)
  soccer_data$HomeTeam <- as.factor(soccer_data$HomeTeam)
  soccer_data$AwayTeam <- as.factor(soccer_data$AwayTeam)
  soccer_data$Referee <- as.factor(soccer_data$Referee)

  soccer_data$Time <- as.POSIXct(soccer_data$Time, format = "%H:%M:%S")

  soccer_data <- soccer_data %>%
    mutate(Stadium = replace(Stadium, Stadium == "American Express Stadium", "The American Express Community Stadium"))
  soccer_data <- soccer_data %>%
    mutate(Stadium = replace(Stadium, Stadium == "American Express Community's Stadium", "The American Express Community Stadium"))

  next_data <- soccer_data  %>%
    filter(Status != "FT")

  soccer_data <- soccer_data %>%
    filter(Status == "FT")
  model_data <- soccer_data %>%
    select(HomeTeam, AwayTeam, Stadium, Time, GoalDiff)

  set.seed(123)  # for reproducibility
  split_index <- createDataPartition(model_data$GoalDiff, p = 0.8, list = FALSE)
  train_data <- model_data[split_index, ]
  test_data <- model_data[-split_index, ]
  tail(model_data, 20)

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
  # precision <- accuracy(predictions_lm, test_data$GoalDiff)
  # recall <- recall(predictions_lm, test_data$GoalDiff)

  mse_lm <- mean((predictions_lm - test_data$GoalDiff)^2)
  rmse_lm <- sqrt(mse_lm)
  mae_lm <- mean(abs(predictions_lm - test_data$GoalDiff))

  # cat("\nAccuracy:", accuracy)
  # cat("\nPrecision:", precision)
  # cat("\nRecall:", recall, "\n")
  # cat("Regression Metrics for Linear Regression model:\n")
  # cat("Mean Squared Error (MSE):", mse_lm, "\n")
  # cat("Root Mean Squared Error (RMSE):", rmse_lm, "\n")
  # cat("Mean Absolute Error (MAE):", mae_lm, "\n")

  predictions_next <- predict(lm_model, newdata = next_data)

  predictions_next <- round(predictions_next)

  next_data$GoalDiff <- predictions_next
  next_data$Home_Winner <- ifelse(next_data$GoalDiff > 0, TRUE,
                                  ifelse(next_data$GoalDiff < 0, FALSE, NA))

  match_result <- rbind(soccer_data, next_data)
  return(match_result)
}
