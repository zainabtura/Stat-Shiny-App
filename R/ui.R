build_nav_button <- function(id, label, icon_name) {
  actionButton(
    id,
    label = tagList(icon(icon_name), span(class = "nav-label", label)),
    class = "nav-button",
    title = label
  )
}

build_study_card <- function(id, label, icon_name, description) {
  actionButton(
    id,
    label = tagList(
      div(class = "study-card-icon", icon(icon_name)),
      div(class = "study-card-title", label),
      div(class = "study-card-desc", description)
    ),
    class = "study-card"
  )
}

build_home_feature_card <- function(id, label, icon_name) {
  actionButton(
    id,
    label = tagList(
      div(class = "home-feature-icon", icon(icon_name)),
      div(class = "home-feature-title", label)
    ),
    class = "home-feature-card"
  )
}

build_home_study_card <- function(id, label, icon_name, description, badge = NULL, subtitle = NULL) {
  div(
    id = id,
    class = "home-study-card home-static-card",
    tagList(
      if (!is.null(badge)) div(class = "home-study-badge", badge),
      div(class = "home-study-icon", icon(icon_name)),
      div(class = "home-study-title", label),
      if (!is.null(subtitle)) div(class = "home-study-subtitle", subtitle),
      div(class = "home-study-desc", description)
    )
  )
}

build_home_catalog_item <- function(title, description) {
  div(
    class = "home-catalog-item",
    div(class = "home-catalog-title", title),
    div(class = "home-catalog-desc", description)
  )
}

build_home_flow_step <- function(number, icon_name, title, description) {
  div(
    class = "home-flow-step",
    div(
      class = "home-flow-circle",
      div(class = "home-flow-step-number", number),
      div(class = "home-flow-step-icon", icon(icon_name))
    ),
    div(class = "home-flow-step-title", title),
    div(class = "home-flow-step-desc", description)
  )
}

build_home_panel <- function() {
  div(
    class = "home-panel",
    div(
      class = "home-shell",
      div(
        class = "home-hero-panel",
        div(
          class = "home-hero-main",
          div(class = "home-kicker", "Universal Data Analyzer"),
          h2("A clearer, faster way to move from raw data to statistical insight."),
          p("Upload your dataset, scan its structure, explore variables, run inferential procedures, fit regression models, and verify model assumptions from one workspace."),
          div(
            class = "home-hero-actions",
            actionButton(
              "home_study_upload",
              label = tagList(icon("upload"), "Upload Dataset"),
              class = "home-hero-button home-hero-button-primary"
            ),
            tags$button(
              id = "home_start_exploring",
              type = "button",
              class = "btn home-hero-button home-hero-button-secondary home-static-button",
              "Start Exploring"
            )
          ),
          div(
            class = "home-hero-note",
            "Upload a CSV or Excel file first, then move into the study that matches your question."
          )
        )
      ),
      div(
        class = "home-flow-band",
        div(class = "home-status-label", "How To Use"),
        div(
          class = "home-flow-track",
          build_home_flow_step("1", "upload", "Upload", "Securely ingest structured or unstructured datasets and get your workspace ready."),
          build_home_flow_step("2", "table-columns", "Review", "Scan summaries, preview the data, and understand structure and quality."),
          build_home_flow_step("3", "chart-line", "Run", "Apply descriptive, inferential, and regression workflows to answer your questions."),
          build_home_flow_step("4", "shield-halved", "Check", "Evaluate assumptions, inspect diagnostics, and correct model issues.")
        )
      ),
      div(
        class = "home-section home-study-section",
        div(
          class = "home-section-head",
          div(
            class = "home-section-heading-block",
            div(class = "home-section-title", "Core Studies"),
            div(class = "home-section-caption", "Choose the main direction of analysis and move into focused outputs immediately.")
          )
        ),
        div(
          class = "home-study-grid",
          build_home_study_card(
            "home_study_descriptive",
            "Explore",
            "chart-column",
            "Summarize the dataset, inspect variable structure, and explore numeric or categorical patterns.",
            subtitle = "Descriptive Analysis"
          ),
          build_home_study_card(
            "home_study_tests",
            "Test",
            "flask",
            "Run one-sample, two-sample, and distribution-based procedures on numeric variables.",
            subtitle = "Inferential Statistics"
          ),
          build_home_study_card(
            "home_study_regression",
            "Model",
            "chart-line",
            "Fit simple or multiple linear regression models and inspect diagnostics and model quality.",
            subtitle = "Regression"
          ),
          build_home_study_card(
            "home_study_adequacy",
            "Validate",
            "shield-halved",
            "Evaluate assumptions like normality, homoscedasticity, independence, and linearity.",
            subtitle = "Model Adequacy"
          )
        )
      ),
      div(
        class = "home-section home-catalog-section",
        div(
          class = "home-section-head",
          div(
            class = "home-section-heading-block",
            div(class = "home-section-title", "Other Details"),
            div(class = "home-section-caption", "The extended analysis catalog that supports the complete workflow.")
          ),
          div(
            class = "home-catalog-intro",
            "Use this as a quick map of the full analysis workflow."
          )
        ),
        div(
          class = "home-catalog-grid",
          build_home_catalog_item("Summary", "Provides a quick overview of the dataset including number of rows, columns, missing values, variable types, and strongest correlations."),
          build_home_catalog_item("Data Preview", "Displays the uploaded dataset and basic information about its structure and data quality."),
          build_home_catalog_item("Single Numeric", "Analyzes one numeric variable using descriptive statistics, histograms, and boxplots."),
          build_home_catalog_item("Two Numeric", "Examines relationships between two numeric variables using correlation analysis and scatter plots."),
          build_home_catalog_item("Multi Numeric", "Explores relationships among multiple numeric variables using correlation matrices and scatterplot matrices."),
          build_home_catalog_item("Single Categorical", "Analyzes one categorical variable using frequency tables, bar charts, and pie charts."),
          build_home_catalog_item("Two Categorical", "Compares two categorical variables using contingency tables and graphical comparisons."),
          build_home_catalog_item("Tests", "Performs statistical hypothesis tests including normality tests, one-sample t-tests, Wilcoxon tests, and two-group comparisons."),
          build_home_catalog_item("Regression", "Fits simple or multiple linear regression models depending on the selected predictors and displays model summaries and diagnostics."),
          build_home_catalog_item("Model Adequacy", "Evaluates regression assumptions such as normality, homoscedasticity, independence, and linearity."),
          build_home_catalog_item("Correct Inadequacies", "Provides correction methods such as Box-Cox transformations, Box-Tidwell testing, and Weighted Least Squares."),
          build_home_catalog_item("Multicollinearity", "Detects multicollinearity using VIF and applies Ridge or Lasso regression with cross-validated comparison against OLS."),
          build_home_catalog_item("Model Building", "Performs feature selection with best subset, forward, backward, and stepwise procedures across MSE, Adjusted R², Cp, AIC, and BIC."),
          build_home_catalog_item("Influence Diagnostics", "Detects leverage points and influential observations using Cook's D, DFFITS, and DFBETAs. Compares OLS, cleaned OLS, Huber, and Bisquare robust models."),
          build_home_catalog_item("Polynomial Regression", "Fit linear, quadratic, and cubic polynomial models with and without centering. Compares VIF across models to illustrate multicollinearity reduction."),
          build_home_catalog_item("Spline Regression", "Fit piecewise linear, quadratic, and cubic B-spline models. Users set knot locations interactively and compare fits with residual plots."),
          build_home_catalog_item("GLM: Logit / Probit / Poisson", "Fit logistic regression (logit and probit links) for binary outcomes, and Poisson regression for count data. Includes coefficient tables, LR tests, and prediction.")
        )
      )
    )
  )
}

build_ui <- function() {
  catalog <- study_catalog()
  nav_groups <- unique(catalog$group)

  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        $(document).on('change', '#file', function() {
          var fileName = $(this).val().split('\\\\').pop();
          if (fileName) {
            $('#file-name-display').html('<strong>✓</strong> ' + fileName).addClass('active');
          } else {
            $('#file-name-display').text('').removeClass('active');
          }
        });
      "))
    ),

    div(
      id = "app-shell",
      class = "app-shell",
      div(
        class = "shell-body",
        div(
          class = "study-sidebar",
          div(
            class = "sidebar-scroll",
            div(
              class = "sidebar-toggle-row",
              div(class = "sidebar-brand-title", "Universal Data Analyzer"),
              actionButton("toggle_sidebar", label = icon("bars"), class = "shell-toggle", title = "Toggle sidebar")
            ),
            div(
              class = "sidebar-nav",
              lapply(nav_groups, function(group_name) {
                group_items <- catalog[catalog$group == group_name, , drop = FALSE]

                div(
                  class = "nav-group",
                  div(class = "nav-group-title", group_name),
                  lapply(seq_len(nrow(group_items)), function(i) {
                    item <- group_items[i, ]
                    build_nav_button(item$nav_id, item$label, item$icon)
                  })
                )
              })
            ),

            div(
              class = "sidebar-section utility-section",
              div(class = "section-title", "Actions"),
              actionButton("reset_selections", "Reset Selections", class = "btn btn-secondary btn-block"),
              actionButton("reset_app", "Reset App", class = "btn btn-warning btn-block")
            )
          )
        ),

        div(
          class = "workspace-main",
          div(
            style = "display: none;",
            uiOutput("var_select_ui"),
            numericInput("mu0", "Hypothesized mean (μ₀)", value = 0, step = 0.1),
            checkboxInput("equal_var", "Equal variance", TRUE)
          ),
          tabsetPanel(
            id = "main_tabs",
            type = "hidden",
            selected = "tab_home",

            tabPanel("Home", value = "tab_home", build_home_panel()),
            tabPanel("Dataset Upload", value = "tab_upload", uiOutput("upload_page_ui")),
            tabPanel("Descriptive Analysis", value = "tab_descriptive", uiOutput("descriptive_page_ui")),

            tabPanel(
              "Inferential Statistics",
              value = "tab_tests",
              div(class = "tests-page-shell", withSpinner(uiOutput("tests_out_ui"), color = "#6366f1", type = 4))
            ),

            tabPanel("Regression", value = "tab_regression", uiOutput("regression_ui")),
            tabPanel("Indicator Variables", value = "tab_indicator", uiOutput("indicator_ui")),
            tabPanel("Model Adequacy", value = "tab_adequacy", uiOutput("adequacy_ui")),

            tabPanel(
              "Correct Inadequacies",
              value = "tab_corrections",
              div(
                class = "content-card",
                div(class = "card-header", "📦 Box-Cox Transformation"),
                uiOutput("boxcox_info"),
                withSpinner(plotOutput("boxcox_plot", height = "500px"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "📈 Box-Tidwell Test for Linearity"),
                div(
                  class = "info-card",
                  HTML("Tests whether predictors need power transformations. Non-significant p-values suggest linear relationships are appropriate.")
                ),
                withSpinner(uiOutput("box_tidwell"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "⚖️ Weighted Least Squares (WLS)"),
                div(
                  class = "info-card",
                  HTML("WLS corrects heteroscedasticity by giving less weight to observations with higher variance.")
                ),
                withSpinner(uiOutput("wls_summary"), color = "#6366f1", type = 4)
              )
            ),

            tabPanel(
              "Multicollinearity",
              value = "tab_multicollinearity",
              uiOutput("multicollinearity_page_ui")
            ),

            tabPanel(
              "Model Building",
              value = "tab_model_building",
              div(
                class = "content-card",
                div(class = "card-header", "📋 Full Model Summary"),
                div(
                  class = "info-card",
                  HTML("Traditional approach: fit the full model with all predictors, then evaluate using ANOVA, correlation, and VIF.")
                ),
                withSpinner(uiOutput("mb_full_stats"), color = "#6366f1", type = 4),
                div(class = "table-card", h4("Coefficients"), DTOutput("mb_full_coef")),
                div(class = "table-card", h4("ANOVA Table"), DTOutput("mb_full_anova_tbl"))
              ),
              div(
                class = "content-card",
                div(class = "card-header", "🔗 Pairwise Correlation"),
                div(
                  class = "info-card",
                  HTML("Pairwise correlations between the response and predictors.")
                ),
                withSpinner(plotOutput("mb_corr_plot", height = "480px"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "🧪 Best Subset Selection"),
                div(
                  class = "info-card",
                  HTML("All possible regressions ranked by MSEp, Adjusted R², Predicted R², Mallows' Cp, AIC, and BIC.")
                ),
                withSpinner(uiOutput("mb_best_summary"), color = "#6366f1", type = 4),
                div(class = "table-card", h4("All Possible Regressions"), DTOutput("mb_all_subsets")),
                div(class = "table-card", h4("Mallows' Cp vs p"), withSpinner(plotOutput("mb_cp_plot", height = "420px"), color = "#6366f1", type = 4))
              ),
              div(
                class = "content-card",
                div(class = "card-header", "➡️ Forward Selection"),
                div(
                  class = "info-card",
                  HTML("Forward stepwise selection using p-value entry threshold (penter = 0.10).")
                ),
                withSpinner(uiOutput("mb_forward_ui"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "⬅️ Backward Selection"),
                div(
                  class = "info-card",
                  HTML("Backward stepwise elimination using p-value removal threshold (prem = 0.15).")
                ),
                withSpinner(uiOutput("mb_backward_ui"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "🔄 Stepwise Selection"),
                div(
                  class = "info-card",
                  HTML("Bidirectional stepwise selection (penter = 0.10, prem = 0.15).")
                ),
                withSpinner(uiOutput("mb_stepwise_ui"), color = "#6366f1", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "✅ Recommended Final Model"),
                div(
                  class = "info-card success",
                  HTML("Model selected by minimizing Mallows' |Cp − p|. Compare across criteria above to confirm.")
                ),
                withSpinner(uiOutput("mb_final_stats"), color = "#6366f1", type = 4),
                div(class = "table-card", h4("Final Model Coefficients"), DTOutput("mb_final_coef")),
                div(class = "table-card", h4("Final Model VIF"), DTOutput("mb_final_vif_tbl"))
              )
            ),

            tabPanel("Influence Diagnostics", value = "tab_influence",
              uiOutput("influence_ui")
            ),

            tabPanel("Polynomial Regression", value = "tab_polynomial",
              uiOutput("polynomial_ui")
            ),

            tabPanel("Spline Regression", value = "tab_spline",
              uiOutput("spline_ui")
            ),

            tabPanel("GLM (Logit/Probit/Poisson)", value = "tab_glm",
              uiOutput("glm_ui")
            )
          )
        )
      )
    )
  )
}
