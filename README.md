# Project-Group-6

#### Christopher Mulya - 49209794
#### Natalie Crystal Coutinho - 66731928
#### Pranav Kumar Mahesh - 39703434

[![R Package Test](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml/badge.svg)](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml)


_This documentation outlines the functionalities and usage of the DashKick Analytics application._

_The DashKick Analytics application is an R-based package tailored for predicting and visualizing metrics relevant to the English Premier League Football-related data of the 2023-2024 season._

---

# DashKick Analytics Package

The `dashkick_analytics` package is a robust tool designed for streamlined soccer team management, player performance evaluation, and strategic decision-making. Leveraging advanced analytics and intuitive visualizations, this package provides users with a comprehensive platform.

## Overview

### Restful API

For this project, we chose the API-Football as the Restful API, offering rich soccer-related data.

### Modules

#### Teams Module

The `teams` module provides essential functions for retrieving top goal scorers and making predictions for the 2023 season.

##### `game_changers()`
   - Pulls the top 20 game changers from the Premier League 2023 season.

##### `predict_win()`
   - Predicts the outcomes of the remaining matches of the 2023 season.

##### `predict_league()`
   - Predicts the final league standings of the 2023 season.

#### Visualizations Module

The `visualizations` module facilitates the creation of interactive visualizations.

##### `top_20()`

   - The top_20 function generates a stacked bar plot displaying the top 20 goal scorers in the Premier League 2023 season.


## Getting Started

To get started with the DashKick Analytics package, install it using the following:

```r
# Install from GitHub
devtools::install_github("nccoutinho/DashKick_Analytics/dashkickAnalytics")
```

## Conclusion

The `dashkick_analytics` package simplifies soccer team analysis, providing valuable insights and predictive capabilities. Explore the documentation for detailed function descriptions and parameters.

For more information, visit our GitHub repository: [DashKick Analytics GitHub Repo](https://github.com/nccoutinho/DashKick_Analytics).
