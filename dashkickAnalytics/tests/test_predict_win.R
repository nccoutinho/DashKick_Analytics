library(testthat)
library(dashkickAnalytics)

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
