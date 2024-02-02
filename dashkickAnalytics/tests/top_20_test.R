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