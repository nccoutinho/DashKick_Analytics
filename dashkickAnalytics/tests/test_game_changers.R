library(testthat)
library(webmockr)
library(tidyjson)
library(dplyr)
library(jsonlite)
library(httr)
library(tidyjson)
library(dplyr)
library(jsonlite)
library(DT)
library(plotly)
library(dashkickAnalytics)
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

