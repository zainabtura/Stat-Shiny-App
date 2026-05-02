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
          build_home_catalog_item("Correct Inadequacies", "Provides correction methods such as Box-Cox transformations, Box-Tidwell testing, and Weighted Least Squares.")
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
        class = "shell-topbar",
        div(
          class = "topbar-left",
          actionButton("toggle_sidebar", label = icon("bars"), class = "shell-toggle", title = "Toggle sidebar"),
          div(
            class = "brand-lockup",
            div(class = "brand-title", "Universal Data Analyzer"),
            div(class = "brand-subtitle", "Home-first navigation for your studies")
          )
        ),
        uiOutput("topbar_status_ui")
      ),

      div(
        class = "shell-body",
        div(
          class = "study-sidebar",
          div(
            class = "sidebar-scroll",
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
              div(class = "tests-page-shell", withSpinner(uiOutput("tests_out_ui"), color = "#0f766e", type = 4))
            ),

            tabPanel("Regression", value = "tab_regression", uiOutput("regression_ui")),
            tabPanel("Model Adequacy", value = "tab_adequacy", uiOutput("adequacy_ui")),

            tabPanel(
              "Correct Inadequacies",
              value = "tab_corrections",
              div(
                class = "content-card",
                div(class = "card-header", "📦 Box-Cox Transformation"),
                uiOutput("boxcox_info"),
                withSpinner(plotOutput("boxcox_plot", height = "500px"), color = "#0f766e", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "📈 Box-Tidwell Test for Linearity"),
                div(
                  class = "info-card",
                  HTML("Tests whether predictors need power transformations. Non-significant p-values suggest linear relationships are appropriate.")
                ),
                withSpinner(uiOutput("box_tidwell"), color = "#0f766e", type = 4)
              ),
              div(
                class = "content-card",
                div(class = "card-header", "⚖️ Weighted Least Squares (WLS)"),
                div(
                  class = "info-card",
                  HTML("WLS corrects heteroscedasticity by giving less weight to observations with higher variance.")
                ),
                withSpinner(uiOutput("wls_summary"), color = "#0f766e", type = 4)
              )
            )
          )
        )
      )
    )
  )
}
