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
        class = "hero-section",
        div(class = "hero-bg-shape hero-bg-1"),
        div(class = "hero-bg-shape hero-bg-2"),
        div(
          class = "hero-inner",
          div(class = "hero-badge-row",
            div(class = "hero-badge", icon("chart-simple"), span("Statistical Analysis Platform"))
          ),
          h1(class = "hero-title",
            "Analyze Your Data",
            br(),
            span(class = "hero-accent", "With Confidence")
          ),
          p(class = "hero-subtitle",
            "Upload datasets, run descriptive and inferential analyses, build regression models, and validate assumptions — all from a single workspace."
          ),
          div(
            class = "hero-cta-row",
            actionButton(
              "home_study_upload",
              label = tagList(icon("arrow-up-from-bracket"), "Upload Dataset"),
              class = "hero-cta-primary"
            ),
            div(class = "hero-cta-hint", icon("circle-info"), span("Supports CSV and Excel files"))
          )
        )
      ),

      div(
        class = "preview-bar",
        div(class = "preview-stat",
          div(class = "preview-stat-icon", icon("table-columns")),
          div(class = "preview-stat-text",
            div(class = "preview-stat-num", "17"),
            div(class = "preview-stat-label", "Analysis Modules")
          )
        ),
        div(class = "preview-divider"),
        div(class = "preview-stat",
          div(class = "preview-stat-icon", icon("chart-line")),
          div(class = "preview-stat-text",
            div(class = "preview-stat-num", "4"),
            div(class = "preview-stat-label", "Core Workflows")
          )
        ),
        div(class = "preview-divider"),
        div(class = "preview-stat",
          div(class = "preview-stat-icon", icon("arrows-rotate")),
          div(class = "preview-stat-text",
            div(class = "preview-stat-num", "End-to-End"),
            div(class = "preview-stat-label", "Upload to Validation")
          )
        )
      ),

      div(
        class = "bento",
        actionButton(
          "home_nav_descriptive",
          label = tagList(
            div(class = "bento-visual",
              div(class = "bento-mini-bar", style = "height:60%"),
              div(class = "bento-mini-bar", style = "height:85%"),
              div(class = "bento-mini-bar", style = "height:45%"),
              div(class = "bento-mini-bar", style = "height:70%"),
              div(class = "bento-mini-bar", style = "height:90%")
            ),
            div(class = "bento-tag", "Descriptive"),
            div(class = "bento-heading", "Explore Your Data"),
            div(class = "bento-copy", "Summaries, distributions, histograms, boxplots, and correlation analysis across all variable types.")
          ),
          class = "bento-card bento-featured"
        ),
        actionButton(
          "home_nav_tests",
          label = tagList(
            div(class = "bento-icon-block bento-icon-blue", icon("flask-vial")),
            div(class = "bento-tag", "Inferential"),
            div(class = "bento-heading", "Test Hypotheses"),
            div(class = "bento-copy", "t-tests, Wilcoxon, normality checks, and two-group comparisons.")
          ),
          class = "bento-card"
        ),
        actionButton(
          "home_nav_regression",
          label = tagList(
            div(class = "bento-icon-block bento-icon-indigo", icon("chart-line")),
            div(class = "bento-tag", "Regression"),
            div(class = "bento-heading", "Build Models"),
            div(class = "bento-copy", "Simple and multiple linear regression with full diagnostics.")
          ),
          class = "bento-card"
        ),
        actionButton(
          "home_nav_adequacy",
          label = tagList(
            div(class = "bento-visual bento-visual-check",
              div(class = "bento-check-row", icon("circle-check"), span("Normality")),
              div(class = "bento-check-row", icon("circle-check"), span("Homoscedasticity")),
              div(class = "bento-check-row", icon("circle-xmark"), span("Independence"))
            ),
            div(class = "bento-tag", "Adequacy"),
            div(class = "bento-heading", "Validate Assumptions"),
            div(class = "bento-copy", "Check every regression assumption and identify violations.")
          ),
          class = "bento-card bento-featured"
        )
      ),

      div(
        class = "more-section",
        div(class = "more-header",
          h3("More Analysis Tools"),
          p("Advanced modules that extend the core workflow.")
        ),
        div(
          class = "more-grid",

          actionButton("home_nav_corrections", label = tagList(
            div(class = "more-icon more-icon-amber", icon("wrench")),
            div(class = "more-body", div(class = "more-title", "Correct Inadequacies"), div(class = "more-desc", "Box-Cox, Box-Tidwell, WLS"))
          ), class = "more-card"),

          actionButton("home_nav_multicollinearity", label = tagList(
            div(class = "more-icon more-icon-rose", icon("diagram-project")),
            div(class = "more-body", div(class = "more-title", "Multicollinearity"), div(class = "more-desc", "VIF, Ridge, Lasso"))
          ), class = "more-card"),

          actionButton("home_nav_model_building", label = tagList(
            div(class = "more-icon more-icon-teal", icon("cubes")),
            div(class = "more-body", div(class = "more-title", "Model Building"), div(class = "more-desc", "Best subset, stepwise selection"))
          ), class = "more-card"),

          actionButton("home_nav_influence", label = tagList(
            div(class = "more-icon more-icon-orange", icon("magnifying-glass-chart")),
            div(class = "more-body", div(class = "more-title", "Influence Diagnostics"), div(class = "more-desc", "Cook's D, DFFITS, DFBETAs"))
          ), class = "more-card"),

          actionButton("home_nav_polynomial", label = tagList(
            div(class = "more-icon more-icon-cyan", icon("wave-square")),
            div(class = "more-body", div(class = "more-title", "Polynomial Regression"), div(class = "more-desc", "Quadratic, cubic, centering"))
          ), class = "more-card"),

          actionButton("home_nav_spline", label = tagList(
            div(class = "more-icon more-icon-emerald", icon("bezier-curve")),
            div(class = "more-body", div(class = "more-title", "Spline Regression"), div(class = "more-desc", "B-splines with interactive knots"))
          ), class = "more-card"),

          actionButton("home_nav_glm", label = tagList(
            div(class = "more-icon more-icon-violet", icon("square-binary")),
            div(class = "more-body", div(class = "more-title", "GLM"), div(class = "more-desc", "Logistic, probit, Poisson"))
          ), class = "more-card")
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
