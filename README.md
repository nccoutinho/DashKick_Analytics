# Project-Group-6

#### Christopher Mulya - 49209794
#### Natalie Crystal Coutinho - 66731928
#### Pranav Kumar Mahesh - 39703434

[![R Package Test](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml/badge.svg)](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml)


_This documentation outlines the functionalities and usage of the DashKick Analytics application._

_The DashKick Analytics application is an R-based package tailored for predicting and visualizing metrics relevant to the English Premier League Football-related data of the 2023-24 season._

---

# DashKick Analytics Package

The `dashkick_analytics` package is a robust tool designed for streamlined soccer team management, player performance evaluation, and strategic decision-making. Leveraging advanced analytics and intuitive visualizations, this package provides users with a comprehensive platform.

## Overview

### Restful API

For this project, we chose the API-Football as the Restful API, offering rich soccer-related data.

### Modules

#### Teams Module

The `teams` module provides essential functions for retrieving top goal scorers and making predictions for the Premier League 2023-24 season.

##### `game_changers()`
   - This function retrieves information about the top 20 game changers from the Premier League 2023-24 season. Game changers are players who have significantly impacted matches with their performances, contributing to crucial goals and influencing the outcome. This may include player names, teams, and relevant statistics.

##### `predict_win()`
   - Predicts the outcomes of the remaining matches in the Premier League 2023 season. This function utilizes advanced algorithms and historical performance data to provide insights into the potential winners of upcoming matches.

##### `predict_league()`
   - Predicts the final league standings of the Premier League 2023 season.

#### Visualizations Module

The `visualizations` module facilitates the creation of interactive visualizations, such as stacked bar plot for top goal scorers, spider plots for team comparisons, and line plots for historical performance, exclusively focusing on the insights derived from the 2023 Premier League season. 

##### `top_20()`

   - The top_20 function generates a stacked bar plot displaying the top 20 goal scorers in the Premier League 2023 season.

##### `compare_teams(team1, team2)`

   - The compare_teams function creates a spider plot that compares performance metrics between two teams. This visualization allows users to assess various aspects of team performance, such as goals for, goals against, draws, and losses, providing a dynamic and intuitive way to compare teams head-to-head.

##### `track_performance(team)`

   - The track_performance function illustrates a team's performance over time with a multi-line plot. By visualizing the number of goals made by a team across multiple matches, users can identify trends, assess consistency, and gain a deeper understanding of a team's strengths and areas for improvement.


## Getting Started

To get started with the DashKick Analytics package, install it using the following prompt:

```r
# Install from GitHub
devtools::install_github("nccoutinho/DashKick_Analytics/dashkickAnalytics")
```

## Conclusion

The `dashkick_analytics` package simplifies soccer team analysis, providing valuable insights and predictive capabilities. Explore the documentation for detailed function descriptions and parameters.

For more information, visit our GitHub repository: [DashKick Analytics GitHub Repo](https://github.com/nccoutinho/DashKick_Analytics).
