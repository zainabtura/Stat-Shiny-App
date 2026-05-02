library(shiny)
library(readr)
library(readxl)
library(DT)
library(moments)
library(corrplot)
library(RColorBrewer)
library(plotrix)
library(shinycssloaders)
library(shinyjs)
library(car)
library(lmtest)
library(nortest)
library(randtests)
library(MASS)

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is_categorical <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  format(round(p, 4), nsmall = 4)
}

missing_pct <- function(x) {
  round(sum(is.na(x)) / length(x) * 100, 1)
}

get_top_correlations <- function(df, n = 3) {
  nums <- df[, sapply(df, is.numeric), drop = FALSE]
  if (ncol(nums) < 2) return(NULL)

  cm <- cor(nums, use = "complete.obs")
  cm[upper.tri(cm, diag = TRUE)] <- NA

  pairs <- which(!is.na(cm), arr.ind = TRUE)
  if (nrow(pairs) == 0) return(NULL)

  cors <- data.frame(
    Var1 = rownames(cm)[pairs[, 1]],
    Var2 = colnames(cm)[pairs[, 2]],
    Correlation = cm[pairs],
    stringsAsFactors = FALSE
  )

  cors <- cors[order(abs(cors$Correlation), decreasing = TRUE), ]
  head(cors, n)
}

study_catalog <- function() {
  data.frame(
    tab = c(
      "tab_home",
      "tab_upload",
      "tab_descriptive",
      "tab_tests",
      "tab_regression",
      "tab_indicator",
      "tab_adequacy",
      "tab_corrections",
      "tab_multicollinearity",
      "tab_model_building"
    ),
    nav_id = c(
      "nav_home",
      "nav_upload",
      "nav_descriptive",
      "nav_tests",
      "nav_regression",
      "nav_indicator",
      "nav_adequacy",
      "nav_corrections",
      "nav_multicollinearity",
      "nav_model_building"
    ),
    card_id = c(
      NA,
      NA,
      NA,
      "card_tests",
      NA,
      NA,
      "card_adequacy",
      "card_corrections",
      "card_multicollinearity",
      "card_model_building"
    ),
    label = c(
      "Home",
      "Dataset Upload",
      "Descriptive Analysis",
      "Inferential Statistics",
      "Regression",
      "Indicator Variables",
      "Model Adequacy",
      "Correct Inadequacies",
      "Multicollinearity",
      "Model Building"
    ),
    icon = c(
      "home",
      "upload",
      "list-alt",
      "flask",
      "line-chart",
      "tags",
      "search",
      "wrench",
      "diagram-project",
      "sitemap"
    ),
    group = c(
      "Overview",
      "Overview",
      "Explore",
      "Explore",
      "Model",
      "Model",
      "Model",
      "Model",
      "Model",
      "Model"
    ),
    description = c(
      "Welcome screen and study selection",
      "Upload a CSV or Excel file for analysis",
      "General summary, single-variable, two-variable, and three-variable descriptive views",
      "Normality and location-parameter inference workflows",
      "Fit and inspect simple or multiple linear regression models",
      "Incorporate categorical predictors as dummy variables in regression models",
      "Check regression assumptions and diagnostics",
      "Box-Cox, Box-Tidwell, and WLS corrections",
      "Detect multicollinearity with VIF and apply ridge or lasso regression",
      "Feature selection via best subset and stepwise procedures"
    ),
    stringsAsFactors = FALSE
  )
}
