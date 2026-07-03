# Universal Data Analyzer

A comprehensive R Shiny application for statistical analysis. Upload a dataset and run descriptive statistics, inferential tests, regression models, and diagnostics from a single workspace.

**Live App:** [universalanalyzer.shinyapps.io/STAT413-TEAM2](https://universalanalyzer.shinyapps.io/STAT413-TEAM2/)

## Features

- **Descriptive Analysis** -- Summaries, distributions, histograms, boxplots, and correlation matrices
- **Inferential Statistics** -- t-tests, Wilcoxon tests, normality checks, and multi-group comparisons
- **Linear Regression** -- Simple and multiple regression with full coefficient tables and ANOVA
- **Indicator Variables** -- Incorporate categorical predictors as dummy variables
- **Model Adequacy** -- Residual diagnostics, normality, homoscedasticity, and independence checks
- **Correct Inadequacies** -- Box-Cox, Box-Tidwell, and Weighted Least Squares
- **Multicollinearity** -- VIF analysis, Ridge, and Lasso regression
- **Model Building** -- Best subset selection, forward/backward/stepwise procedures
- **Influence Diagnostics** -- Cook's D, DFFITS, DFBETAs, and robust regression
- **Polynomial Regression** -- Linear, quadratic, and cubic models with centering
- **Spline Regression** -- B-spline models with user-defined knots
- **GLM** -- Logistic (logit/probit) and Poisson regression with diagnostics

## Run Locally

### Prerequisites

- [R](https://cran.r-project.org/) (>= 4.0)
- Required R packages:

```r
install.packages(c(
  "shiny", "readr", "readxl", "DT", "moments", "corrplot",
  "RColorBrewer", "plotrix", "shinycssloaders", "shinyjs",
  "car", "lmtest", "nortest", "randtests", "MASS", "splines"
))
```

### Start the App

```r
shiny::runApp(".")
```

Or from the terminal:

```bash
Rscript -e "shiny::runApp('.', port = 3838, launch.browser = TRUE)"
```

The app opens at [http://localhost:3838](http://localhost:3838).

## Project Structure

```
.
├── app.R                  # Entry point
├── R/
│   ├── helpers.R          # Utility functions and UI builders
│   ├── styles.R           # CSS theme
│   ├── ui.R               # Main UI layout
│   ├── server.R           # Server composition root
│   ├── server_state.R     # Centralized reactive state
│   ├── server_analysis.R  # Descriptive & inferential modules
│   ├── server_regression.R# Regression module
│   ├── server_indicator.R # Indicator variables module
│   ├── server_influence.R # Influence diagnostics module
│   ├── server_polynomial.R# Polynomial regression module
│   ├── server_spline.R    # Spline regression module
│   └── server_glm.R       # GLM module
└── datasets/              # Built-in sample datasets
```

## Deployment

Deployed on [shinyapps.io](https://www.shinyapps.io/) via the `rsconnect` package:

```r
rsconnect::deployApp(".", appName = "STAT413-TEAM2", account = "universalanalyzer")
```

## Team

STAT 413 -- Team 2
