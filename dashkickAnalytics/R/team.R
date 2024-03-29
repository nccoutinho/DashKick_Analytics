#' Game Changers
#'
#' Retrieve statistics of the top soccer players from the Premier League 2023-23 season.
#'
#' This function retrieves statistics of soccer players from the EPL 2023-24 season
#' using API-Football. It then processes the data to extract relevant information
#' such as player name, team, age, appearances, goals, assists, and penalty goals.
#' It calculates the total goals and assists combined, sorts the players based on this
#' combined statistic and selects the top 20 players.
#'
#'
#' @return A data frame containing statistics of the top 20 football players based on their combined goals and assists.
#' @examples
#' players_data <- game_changers()
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

      tryCatch({
        response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))
        json_string <- content(response, "text", encoding='UTF-8')
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

        all_players <- rbind(rbind(all_players, df))
    }, error = function(e) {
      message('An error occurred while fetching data: ', e$message)
    })
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



#' Predict Win
#'
#' Predicts the outcomes of the remaining matches in the Premier League 2023-24 season.
#'
#' This function retrieves football match data from API - Football, by training a linear regression model on historical data, and predicts match outcomes for upcoming fixtures.
#'
#' @return A data frame containing match details and predicted outcomes.
#' @examples
#' match_data <- predict_win()
#' head(match_data)
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
  tryCatch({
     url <- "https://api-football-v1.p.rapidapi.com/v3/fixtures"

    queryString <- list(
      league = "39",
      season = "2023"
    )

    response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

    json_string <- content(response, "text", encoding='UTF-8')

    json_data <- fromJSON(json_string)

    json_data <- fromJSON(json_string)

    match_stats <- json_data$response
  }, error = function(e){
    message('An error occured: ', e$message)
    return (NULL)
  })



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




#' Predict Final Standings
#'
#' Predicts the final league standings of the Premier League 2023-24 season.
#'
#'This function combines match prediction results with current league standings to generate the final league table. It retrieves match data and standings from specified APIs, performs calculations, and produces a visual representation of the league table.
#'
#' @return A data frame containing the final league standings.
#' @examples
#' final_standings <- predict_final_standings()
#' head(final_standings)
#'
#'
#'
#' @import httr
#' @import jsonlite
#' @import DT
#' @import tidyr
#' @import caret
#' @import nnet
#' @import Metrics
#' @import knitr
#' @import kableExtra
#'
#'
#' @export
predict_final_standings <- function() {
  tryCatch({
  match_result <- predict_win()

  url <- "https://api-football-v1.p.rapidapi.com/v3/standings"

  queryString <- list(
    season = "2023",
    league = "39")

  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

  json_string <- content(response, "text", encoding='UTF-8')

  json_data <- fromJSON(json_string)

  standings <- json_data$response
  }, error = function(e){
    message('An error occured: ', e$message)
    return (NULL)
  })


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
