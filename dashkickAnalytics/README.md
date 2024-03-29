# Project-Group-6

#### Christopher Mulya - 49209794
#### Natalie Crystal Coutinho - 66731928
#### Pranav Kumar Mahesh - 39703434

[![R Package Test](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml/badge.svg)](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml)


_This documentation outlines the functionalities and usage of the DashKick Analytics application._

_The DashKick Analytics application is an R-based package tailored for predicting and visualizing metrics relevant to the English Premier League Football-related data of the 2023-24 season._

---

# DashKick Analytics Package

The `dashkickAnalytics` package is a robust tool designed for streamlined soccer team management, player performance evaluation, and strategic decision-making. Leveraging advanced analytics and intuitive visualizations, this package provides users with a comprehensive platform.

## Getting Started

To get started with the DashKick Analytics package, install it using the following prompt:

```r
# Install dependent libraries manually
# install.packages(c('testthat', 'devtools', 'webmockr', 'httr', 'tidyjson', 'nnet', 'caret', 'ggplot2', 'dplyr', 'jsonlite', 'tidyr', 'Metrics', 'knitr', 'kableExtra', 'DT', 'plotly'), repos='https://cloud.r-project.org/')
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

# Install from GitHub
library(devtools)
devtools::install_github("nccoutinho/DashKick_Analytics/dashkickAnalytics")
```


## Overview

### Restful API

For this project, we chose the API-Football as the Restful API, offering rich soccer-related data.

### Modules

#### Teams Module

The `teams` module provides essential functions for retrieving top goal scorers while also predicting the match outcomes and final league table for the Premier League 2023-24 season.

##### `game_changers()`
   - This function retrieves information about the top 20 game changers from the Premier League 2023-24 season. Game changers are players who have significantly impacted matches with their performances, contributing to crucial goals and influencing the outcome. This includes player names, teams, and relevant statistics.

##### `predict_win()`
   - Predicts the outcomes of the remaining matches in the Premier League 2023-24 season. This function utilizes advanced algorithms and historical performance data to provide insights into the potential winners of upcoming matches. It outputs a data structure containing predictions for the outcomes of all the matches including the remaining matches in the Premier League 2023-24 season.

##### `predict_final_standings()`
   - Predicts the final league standings of the Premier League 2023-24 season. This function considers various factors, including team performance, player statistics, and historical data, to generate a projection of how teams will rank at the end of the season.

#### Visualizations Module

The `visualizations` module facilitates the creation of interactive visualizations, such as stacked bar plot for top goal scorers, spider plots for team comparisons, and line plots for historical performance, exclusively focusing on the insights derived from the 2023 Premier League season. 

##### `top_20()`

   - Utilizing the data retrieved from the game_changers function, the top_20 function generates a stacked bar plot, illustrating the prolific goal scoring prowess of the top 20 performers in the Premire League of the 2023-24 season. 

##### `compare_teams(team1, team2)`

   - The compare_teams function creates a spider plot that compares performance metrics between two teams. This visualization allows users to assess various aspects of team performance, such as goals for, goals against, draws, and losses, providing a dynamic and intuitive way to compare teams head-to-head.

##### `track_performance(team)`

   - The track_performance function illustrates a team's performance over time with a multi-line plot. By visualizing the number of goals made by a team across multiple matches, users can identify trends, assess consistency, and gain a deeper understanding of a team's strengths and areas for improvement.


## Coverage Report

![Coverage Report](https://github.com/nccoutinho/DashKick_Analytics/blob/main/Coverage%20Report.png)

## Conclusion

The `dashkick_analytics` package simplifies soccer team analysis, providing valuable insights and predictive capabilities. Explore the documentation for detailed function descriptions and parameters.

For more information, visit our GitHub repository: [DashKick Analytics GitHub Repo](https://github.com/nccoutinho/DashKick_Analytics).
