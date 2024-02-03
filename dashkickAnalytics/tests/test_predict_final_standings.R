library(testthat)
library(dashkickAnalytics)

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

