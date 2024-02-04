# DashKick Analytics 

#### Christopher Mulya - 49209794
#### Natalie Crystal Coutinho - 66731928
#### Pranav Kumar Mahesh - 39703434

[![R Package Test](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml/badge.svg)](https://github.com/nccoutinho/DashKick_Analytics/actions/workflows/dashkick_test_workflow.yml)


_This documentation outlines the functionalities and usage of the DashKick Analytics application's._



---

# DashKick Analytics Package

The `dashkick_analytics` package is a robust tool designed for streamlined football team management, player performance evaluation, and strategic decision-making. Leveraging advanced analytics and intuitive visualizations, this package provides users with a comprehensive platform.

## Overview

### Restful API

For this project, we chose the API-Football as the Restful API, offering rich football-related data.

### Modules

#### Teams Module

The `teams` module provides essential functions for retrieving top goal scorers and making predictions for the 2023 season.

1. **game_changers:**
   - Pulls the top 20 game changers from the Premier League 2023 season.

2. **predict_win:**
   - Predicts the outcomes of the remaining matches of the 2023 season.

3. **predict_league:**
   - Predicts the final league standings of the 2023 season.

#### Visualizations Module

The `visualizations` module facilitates the creation of interactive visualizations.

1. **top_20:**
   - The top_20 function generates a stacked bar plot displaying the top 20 goal scorers in the Premier League 2023 season.


## Getting Started

To get started with the DashKick Analytics package, install it using the following:

```r
# Install from GitHub
devtools::install_github("nccoutinho/DashKick_Analytics/dashkickAnalytics")
```

## Conclusion

The `dashkick_analytics` package simplifies football team analysis, providing valuable insights and predictive capabilities. Explore the documentation for detailed function descriptions and parameters.

For more information, visit our GitHub repository: [DashKick Analytics GitHub Repo](https://github.com/nccoutinho/DashKick_Analytics).
