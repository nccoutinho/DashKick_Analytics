library(httr)
library(tidyjson)
library(dplyr)
library(jsonlite)
library(DT)
library(plotly)

#' Top 20 Function (Internal)
#'
#' Description of what the function does. (This function is not intended for user access.)
#'
#' @return What the function returns.
#' @examples
#' # This function is not meant to be called directly by users.
#'
#' @keywords internal
top_20 <- function(df) {
  show_bar <- plot_ly(df, x = ~Goals, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Goals', marker = list(color = 'mediumpurple1'),
                      hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Goals: %{x}',
                      customdata = ~Team) %>%
    add_trace(x = ~Assists, y = ~reorder(PlayerName, GoalsAssists), type = 'bar', name = 'Assists', marker = list(color = 'lightblue1'),
              hovertemplate = '<b>%{y}</b><br>Team: %{customdata}<br>Assists: %{x}',
              customdata = ~Team) %>%
    layout(title = 'Top Game Changers of the EPL 23-24',
           xaxis = list(title = list(text = 'Count', font = list(family = 'Arial', size = 20), standoff = 20)),
           yaxis = list(title = list(text = 'Players', position = 'top', font = list(family = 'Arial', size = 20), standoff = 5, tickangle = -45)),
           barmode = 'stack')
  
  return (show_bar)
}