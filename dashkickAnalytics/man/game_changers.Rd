% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/team.R
\name{game_changers}
\alias{game_changers}
\title{Game Changers}
\usage{
game_changers()
}
\value{
A data frame containing statistics of the top 20 football players based on their combined goals and assists.
}
\description{
Retrieve statistics of the top soccer players from the Premier League 2023-23 season.
}
\details{
This function retrieves statistics of soccer players from the EPL 2023-24 season
using API-Football. It then processes the data to extract relevant information
such as player name, team, age, appearances, goals, assists, and penalty goals.
It calculates the total goals and assists combined, sorts the players based on this
combined statistic and selects the top 20 players.
}
\examples{
players_data <- game_changers()


}
