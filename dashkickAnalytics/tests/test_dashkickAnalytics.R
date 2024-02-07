library(testthat)
library(webmockr)
library(dplyr)
library(jsonlite)
library(httr)
library(tidyjson)
library(DT)
library(plotly)
library(caret)
library(nnet)
library(Metrics)
library(knitr)
library(kableExtra)
library(dashkickAnalytics)

httr_mock(on = FALSE)


test_that("track_performance function returns a plotly object with correct attributes", {
  # Use example team
  team_name <- "Arsenal"

  # Run the function
  track_performance_result <- track_performance(team_name)

  # Check if the result is a plotly object
  expect_is(track_performance_result, "plotly")

  # Check if layout is not NULL
  expect_false(is.null(track_performance_result$x$layout), "The layout should not be NULL")

  # Check if certain elements are present within the layout
  expect_true("title" %in% names(track_performance_result$x$layout),
              "Title should be present in layout")

  expect_true("xaxis" %in% names(track_performance_result$x$layout),
              "xaxis should be present in layout")

  expect_true("yaxis" %in% names(track_performance_result$x$layout),
              "yaxis should be present in layout")

  expect_true("legend" %in% names(track_performance_result$x$layout),
              "legend should be present in layout")

})


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

  # Check if 'line' is present in the theme
  expect_true("line" %in% names(compare_teams_result$theme),
              "Line should be present in the theme")

  # Check if 'rect' is present in the theme
  expect_true("rect" %in% names(compare_teams_result$theme),
              "Rect should be present in the theme")

  # Check if 'colour' is present in 'line'
  expect_true("colour" %in% names(compare_teams_result$theme$line),
              "Colour should be present in line")

  # Check if 'fill' is present in 'rect'
  expect_true("fill" %in% names(compare_teams_result$theme$rect),
              "Fill should be present in rect")
})



test_that("predict_win function returns a data frame with correct structure", {
  result <- predict_win()

  # Print column names for debugging
  print(colnames(result))

  # Check if the result is a data frame
  expect_is(result, "data.frame")

  # Check if the required columns are present
  required_columns <- c("FixtureDate", "TimeStamp", "Stadium", "Status",
                        "HomeTeam", "AwayTeam", "Home_Winner", "FT_ScoreHome",
                        "FT_scoreAway", "HT_ScoreHome", "HT_scoreAway",
                        "Referee", "GoalDiff", "Time")
  expect_true(all(required_columns %in% colnames(result)))

  # Check if the result has non-zero rows
  expect_true(nrow(result) > 0)

  # Check if Home_Winner column exists
  expect_true("Home_Winner" %in% colnames(result))

  # Check if Home_Winner values are either TRUE, FALSE, or NA
  valid_home_winner_values <- c(TRUE, FALSE, NA)
  expect_true(all(result$Home_Winner %in% valid_home_winner_values))
})


test_that("predict_final_standings function returns a data frame with correct structure", {
  final_stand <- predict_final_standings()

  # Check if the result is a data frame
  expect_is(final_stand, "data.frame")

  # Check if the required columns are present
  required_columns <- c("Team", "Points", "Goal_Difference", "Position_Displacement")
  expect_true(all(required_columns %in% colnames(final_stand)))

  # Check if the result has exactly 20 rows
  expect_equal(nrow(final_stand), 20)

  # You can add more specific checks based on your expectations

  # Check if Position_Displacement is of integer type
  expect_is(final_stand$Position_Displacement, "integer")

  # Check if Points and Goal_Difference are of numeric type
  expect_is(final_stand$Points, "numeric")
  expect_is(final_stand$Goal_Difference, "numeric")
})



# Turned on mocks
httr_mock(on = TRUE)

# Define mock JSON response for your API
players_mock_json <- '{"get":"players","parameters":{"league":"39","season":"2023"},"errors":[],"results":20,"paging":{"current":1,"total":48},"response":[{"player":{"id":54,"name":"Diego Costa","firstname":"Diego","lastname":"da Silva Costa","age":35,"birth":{"date":"1988-10-07","place":"Lagarto","country":"Brazil"},"nationality":"Spain","height":"188 cm","weight":"83 kg","injured":false,"photo":"https://media.api-sports.io/football/players/54.png"},"statistics":[{"team":{"id":39,"name":"Wolves","logo":"https://media.api-sports.io/football/teams/39.png"},"league":{"id":39,"name":"Premier League","country":"England","logo":"https://media.api-sports.io/football/leagues/39.png","flag":"https://media.api-sports.io/flags/gb.svg","season":2023},"games":{"appearences":null,"lineups":null,"minutes":null,"number":null,"position":"Attacker","rating":null,"captain":false},"substitutes":{"in":null,"out":null,"bench":null},"shots":{"total":null,"on":null},"goals":{"total":null,"conceded":null,"assists":null,"saves":null},"passes":{"total":null,"key":null,"accuracy":null},"tackles":{"total":null,"blocks":null,"interceptions":null},"duels":{"total":null,"won":null},"dribbles":{"attempts":null,"success":null,"past":null},"fouls":{"drawn":null,"committed":null},"cards":{"yellow":null,"yellowred":null,"red":null},"penalty":{"won":null,"commited":null,"scored":null,"missed":null,"saved":null}}]}]}'
for(i in 1:47) {
  stub_request('get', uri = 'https://api-football-v1.p.rapidapi.com/v3/players') %>%
    wi_th(
      query = list(league = "39", season = "2023", page = i),
      headers = list(
        'Accept' = 'application/json, text/xml, application/xml, */*',
        'X-RapidAPI-Key' = '1272d4dfaamshea38349fbd93df4p178e05jsn2804b1438ab3',
        'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com',
        'Content-Type' = 'application/octet-stream'
      )
    ) %>%
    to_return(
      body = players_mock_json,
      headers = list('Content-Type' = 'application/json; charset=utf-8')
    )
}


test_that("game_changers returns correct data", {
  result <- game_changers()
  # Perform assertions based on the result
  expect_is(result, "data.frame")

  expected_columns <- c("PlayerName", "Team", "PlayerAge", "Appearances", "Goals", "Assists", "PenaltyGoals")
  expect_true(all(expected_columns %in% colnames(result)))

  expect_gt(nrow(result), 0)
  expect_true(all(is.numeric(result$PlayerAge)))
  expect_true(all(result$Goals[0] >= 0))
  expect_true(all(result$Assists[0] >= 0))
})


test_that("top_20 produces a valid plot", {
  # Creating some dummy data for testing
  dummy_data <- data.frame(
    PlayerName = c("Player1", "Player2", "Player3"),
    Goals = c(10, 8, 6),
    Assists = c(5, 3, 7),
    GoalsAssists = c(15, 11, 13),
    Team = c("TeamA", "TeamB", "TeamC")
  )

  # Calling the function
  plot_result <- top_20(dummy_data)

  # Check if the result is not NULL
  expect_false(is.null(plot_result), "The plot result should not be NULL")

  # Check if the result is a plotly object
  expect_is(plot_result, "plotly", "The plot result should be a plotly object")

  # Check if layoutAttrs is not NULL
  expect_false(is.null(plot_result$x$layoutAttrs), "The layoutAttrs should not be NULL")

  # Extract the identifier
  identifier <- names(plot_result$x$layoutAttrs)[1]

  # Check if certain elements are present within the identifier
  expect_true("title" %in% names(plot_result$x$layoutAttrs[[identifier]]))
  expect_true("xaxis" %in% names(plot_result$x$layoutAttrs[[identifier]]))
  expect_true("yaxis" %in% names(plot_result$x$layoutAttrs[[identifier]]))
  expect_true("barmode" %in% names(plot_result$x$layoutAttrs[[identifier]]))

  # Optionally, you can dive deeper into specific elements if needed
  if ("title" %in% names(plot_result$x$layoutAttrs[[identifier]])) {
    expect_true("Top Game Changers of the EPL 23-24" %in% plot_result$x$layoutAttrs[[identifier]]$title)
  }

  if ("xaxis" %in% names(plot_result$x$layoutAttrs[[identifier]])) {
    expect_true("Count" %in% plot_result$x$layoutAttrs[[identifier]]$xaxis$title$text)
  }

  if ("yaxis" %in% names(plot_result$x$layoutAttrs[[identifier]])) {
    expect_true("Players" %in% plot_result$x$layoutAttrs[[identifier]]$yaxis$title$text)
  }

  if ("barmode" %in% names(plot_result$x$layoutAttrs[[identifier]])) {
    barmode <- plot_result$x$layoutAttrs[[identifier]]$barmode
    expect_true(barmode == "stack")
  }
})

httr_mock(on = FALSE)
