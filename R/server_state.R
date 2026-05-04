create_app_state <- function(input, output, session) {
  upload_time <- reactiveVal(NULL)
  current_tab <- reactiveVal("tab_home")
  built_regression <- reactiveVal(NULL)
  selected_sample_dataset <- reactiveVal(NULL)

  sample_dataset_dir <- "datasets"
  sample_dataset_files <- if (dir.exists(sample_dataset_dir)) {
    list.files(
      sample_dataset_dir,
      pattern = "\\.(csv|xlsx|xls)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character(0)
  }

  sample_dataset_choices <- setNames(
    sample_dataset_files,
    tools::file_path_sans_ext(basename(sample_dataset_files))
  )

  data_source_info <- reactive({
    if (!is.null(input$file)) {
      return(list(
        kind = "upload",
        name = input$file$name,
        path = input$file$datapath,
        ext = tolower(tools::file_ext(input$file$name))
      ))
    }

    sample_path <- selected_sample_dataset()
    if (!is.null(sample_path) && file.exists(sample_path)) {
      return(list(
        kind = "sample",
        name = basename(sample_path),
        path = sample_path,
        ext = tolower(tools::file_ext(sample_path))
      ))
    }

    NULL
  })

  data_in <- reactive({
    source_info <- data_source_info()
    req(source_info)

    ext <- source_info$ext

    df <- NULL
    if (ext == "csv") {
      df <- readr::read_csv(source_info$path, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(source_info$path)
    } else {
      validate("Unsupported file type")
    }

    if (isTRUE(input$stringsAsFactors)) {
      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
      }
    }

    df
  })

  observeEvent(data_source_info(), {
    req(data_source_info())
    upload_time(Sys.time())
  }, ignoreInit = TRUE)

  numeric_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is.numeric)]
  })

  categorical_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is_categorical)]
  })

  list(
    upload_time = upload_time,
    current_tab = current_tab,
    built_regression = built_regression,
    data_source_info = data_source_info,
    data_in = data_in,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    sample_dataset_choices = sample_dataset_choices,
    selected_sample_dataset = selected_sample_dataset
  )
}

register_core_server <- function(input, output, session, state) {
  enable_targets <- function(ids, enabled) {
    for (id in ids) {
      if (enabled) {
        shinyjs::enable(id)
      } else {
        shinyjs::disable(id)
      }
    }
  }

  set_active_nav <- function(tab_name) {
    catalog <- study_catalog()
    active_nav <- catalog$nav_id[match(tab_name, catalog$tab)]
    if (is.na(active_nav)) active_nav <- "nav_home"

    shinyjs::runjs(sprintf(
      "document.querySelectorAll('.nav-button').forEach(function(el){el.classList.remove('is-active');});
       var active = document.getElementById('%s');
       if (active) active.classList.add('is-active');",
      active_nav
    ))
  }

  navigation_map <- c(
    nav_home = "tab_home",
    nav_upload = "tab_upload",
    nav_descriptive = "tab_descriptive",
    nav_tests = "tab_tests",
    nav_regression = "tab_regression",
    nav_indicator = "tab_indicator",
    nav_adequacy = "tab_adequacy",
    nav_corrections = "tab_corrections",
    nav_multicollinearity = "tab_multicollinearity",
    nav_model_building = "tab_model_building",
    nav_influence = "tab_influence",
    nav_polynomial = "tab_polynomial",
    nav_spline = "tab_spline",
    nav_glm = "tab_glm",
    home_study_upload = "tab_upload",
    card_tests = "tab_tests",
    card_adequacy = "tab_adequacy",
    card_corrections = "tab_corrections",
    card_multicollinearity = "tab_multicollinearity",
    card_model_building = "tab_model_building"
  )

  lapply(names(navigation_map), function(id) {
    observeEvent(input[[id]], {
      state$current_tab(navigation_map[[id]])
    }, ignoreInit = TRUE)
  })

  observeEvent(input$toggle_sidebar, {
    shinyjs::runjs(
      "document.getElementById('app-shell').classList.toggle('sidebar-collapsed');
       setTimeout(function() {
         if (window.jQuery && $.fn.dataTable) {
           $.fn.dataTable.tables({visible: true, api: true}).columns.adjust().draw(false);
         }
         window.dispatchEvent(new Event('resize'));
       }, 280);"
    )
  })

  observe({
    updateTabsetPanel(session, "main_tabs", selected = state$current_tab())
    set_active_nav(state$current_tab())
    shinyjs::runjs(
      "setTimeout(function() {
         if (window.jQuery && $.fn.dataTable) {
           $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
         }
         window.dispatchEvent(new Event('resize'));
       }, 80);"
    )
  })

  observe({
    has_data <- !is.null(state$data_source_info())
    nums <- if (has_data) state$numeric_vars() else character(0)
    has_built_regression <- !is.null(state$built_regression())

    enable_targets(c("nav_home"), TRUE)
    enable_targets(c("nav_upload", "home_study_upload"), TRUE)
    enable_targets(c("nav_descriptive"), has_data)
    enable_targets(c("nav_tests", "card_tests"), length(nums) >= 1)
    enable_targets(c("nav_regression", "nav_indicator"), length(nums) >= 2)
    enable_targets(c("nav_adequacy", "card_adequacy", "nav_corrections", "card_corrections", "nav_multicollinearity", "card_multicollinearity", "nav_model_building", "card_model_building", "nav_influence", "card_influence"), has_built_regression)
    enable_targets(c("nav_polynomial", "nav_spline"), length(nums) >= 2)
    enable_targets(c("nav_glm"), has_data)
  })

  observeEvent(input$reset_selections, {
    nums <- state$numeric_vars()
    cats <- state$categorical_vars()

    if (length(nums) >= 1) updateSelectInput(session, "num1", selected = nums[1])
    if (length(nums) >= 2) updateSelectInput(session, "num2", selected = nums[2])
    if (length(cats) >= 1) {
      updateSelectInput(session, "cat1", selected = cats[1])
      updateSelectInput(session, "group_var", selected = cats[1])
    }
    if (length(cats) >= 2) updateSelectInput(session, "cat2", selected = cats[2])

    showNotification("✓ Selections reset!", type = "message", duration = 2)
  })

  observeEvent(input$reset_app, {
    session$reload()
  })

  observeEvent(input$file, {
    state$built_regression(NULL)
  }, ignoreInit = TRUE)

  observeEvent(state$selected_sample_dataset(), {
    if (!is.null(state$selected_sample_dataset())) {
      state$built_regression(NULL)
    }
  }, ignoreInit = TRUE)

  output$var_select_ui <- renderUI({
    if (is.null(state$data_source_info())) {
      return(
        div(
          class = "helper-text",
          "Load a dataset on the Dataset Upload page to populate the variable selectors and study settings."
        )
      )
    }

    nums <- state$numeric_vars()
    cats <- state$categorical_vars()

    tagList(
      selectInput("num1", "Numeric X", choices = nums, selected = if (length(nums) >= 1) nums[1] else NULL),
      div(class = "helper-text", "For single & two-sample analysis"),
      br(),
      selectInput("num2", "Numeric Y", choices = nums, selected = if (length(nums) >= 2) nums[2] else if (length(nums) >= 1) nums[1] else NULL),
      div(class = "helper-text", "For correlation & scatter plots"),
      br(),
      selectInput("cat1", "Categorical A", choices = cats, selected = if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "Primary categorical variable"),
      br(),
      selectInput("cat2", "Categorical B", choices = cats, selected = if (length(cats) >= 2) cats[2] else if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "For cross-tabulation"),
      br(),
      selectInput("group_var", "Group", choices = cats, selected = if (length(cats) >= 1) cats[1] else NULL),
      div(class = "helper-text", "For group comparisons")
    )
  })

  output$topbar_status_ui <- renderUI({
    source_info <- state$data_source_info()

    if (is.null(source_info)) {
      div(class = "topbar-status", span("No dataset uploaded"))
    } else {
      div(
        class = "topbar-status",
        HTML(paste0("<strong>", source_info$name, "</strong>")),
        span(paste(nrow(state$data_in()), "rows")),
        span("•"),
        span(paste(ncol(state$data_in()), "columns"))
      )
    }
  })
}

register_dashboard_server <- function(input, output, session, state) {
  output$dashboard_ui <- renderUI({
    req(state$data_in())
    df <- state$data_in()

    nums <- state$numeric_vars()
    cats <- state$categorical_vars()

    n_rows <- nrow(df)
    n_cols <- ncol(df)
    n_numeric <- length(nums)
    n_categorical <- length(cats)

    missing_cols <- sapply(df, function(x) sum(is.na(x)))
    total_missing <- sum(missing_cols)
    missing_pct_total <- round(total_missing / (n_rows * n_cols) * 100, 1)

    top_cors <- get_top_correlations(df, n = 3)

    tagList(
      if (n_numeric > 0) {
        div(
          class = "content-card",
          div(class = "card-header", "📈 Numeric Variables Quick Stats"),
          div(
            class = "visual-stats",
            lapply(seq_len(min(4, n_numeric)), function(i) {
              var_name <- nums[i]
              x <- df[[var_name]]

              div(
                class = "visual-stat-item",
                h4(var_name),
                div(
                  class = "kv",
                  div(class = "k", "Mean"), div(class = "v", round(mean(x, na.rm = TRUE), 2)),
                  div(class = "k", "Median"), div(class = "v", round(median(x, na.rm = TRUE), 2)),
                  div(class = "k", "Q1"), div(class = "v", round(quantile(x, 0.25, na.rm = TRUE), 2)),
                  div(class = "k", "Q3"), div(class = "v", round(quantile(x, 0.75, na.rm = TRUE), 2)),
                  div(class = "k", "Min"), div(class = "v", round(min(x, na.rm = TRUE), 2)),
                  div(class = "k", "Max"), div(class = "v", round(max(x, na.rm = TRUE), 2)),
                  div(class = "k", "Std Dev"), div(class = "v", round(sd(x, na.rm = TRUE), 2)),
                  div(class = "k", "Missing"), div(class = "v", paste0(missing_pct(x), "%"))
                )
              )
            })
          )
        )
      },

      if (n_categorical > 0) {
        div(
          class = "content-card",
          div(class = "card-header", "🏷️ Categorical Variables Distribution"),
          div(
            class = "visual-stats",
            lapply(seq_len(min(3, n_categorical)), function(i) {
              var_name <- cats[i]
              x <- df[[var_name]]
              freq <- table(x, useNA = "ifany")
              top_level <- names(sort(freq, decreasing = TRUE))[1]

              div(
                class = "visual-stat-item",
                h4(var_name),
                div(
                  class = "kv",
                  div(class = "k", "Unique"), div(class = "v", length(freq)),
                  div(class = "k", "Mode"), div(class = "v", as.character(top_level)),
                  div(class = "k", "Frequency"), div(class = "v", as.numeric(freq[top_level]))
                )
              )
            })
          )
        )
      },

      if (!is.null(top_cors)) {
        div(
          class = "content-card",
          div(class = "card-header", "🔗 Top Correlations"),
          lapply(seq_len(nrow(top_cors)), function(i) {
            row <- top_cors[i, ]
            cor_val <- round(row$Correlation, 3)
            cor_class <- if (abs(cor_val) > 0.7) "success" else "info"

            div(
              class = paste("info-card", cor_class),
              HTML(paste0(
                "<div><strong>", row$Var1, " ↔ ", row$Var2, "</strong></div>",
                "<div style='margin-top: 6px;'>Correlation: <strong>", cor_val, "</strong></div>"
              ))
            )
          })
        )
      }
    )
  })
}

register_preview_server <- function(input, output, session, state) {
  observeEvent(input$upload_back_home, {
    state$current_tab("tab_home")
  }, ignoreInit = TRUE)

  observeEvent(input$upload_go_descriptive, {
    state$current_tab("tab_descriptive")
  }, ignoreInit = TRUE)

  observeEvent(input$upload_from_descriptive, {
    state$current_tab("tab_upload")
  }, ignoreInit = TRUE)

  observeEvent(input$load_sample_dataset, {
    req(input$sample_dataset)
    if (nzchar(input$sample_dataset)) {
      state$selected_sample_dataset(input$sample_dataset)
    }
  }, ignoreInit = TRUE)

  mode_button_map <- c(
    descriptive_mode_summary = "General Summary",
    descriptive_mode_single = "Single Variable Analysis",
    descriptive_mode_two = "Two Variables Analysis",
    descriptive_mode_three = "Three Variables Analysis"
  )

  # Keep user selections stable across dynamic UI re-renders.
  last_dataset_var <- reactiveVal(NULL)
  last_dataset_var1 <- reactiveVal(NULL)
  last_dataset_var2 <- reactiveVal(NULL)
  last_dataset_multi_var1 <- reactiveVal(NULL)
  last_dataset_multi_var2 <- reactiveVal(NULL)
  last_dataset_multi_var3 <- reactiveVal(NULL)

  observeEvent(input$dataset_var, {
    if (!is.null(input$dataset_var) && nzchar(input$dataset_var)) {
      last_dataset_var(input$dataset_var)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_var1, {
    if (!is.null(input$dataset_var1) && nzchar(input$dataset_var1)) {
      last_dataset_var1(input$dataset_var1)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_var2, {
    if (!is.null(input$dataset_var2) && nzchar(input$dataset_var2)) {
      last_dataset_var2(input$dataset_var2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_multi_var1, {
    if (!is.null(input$dataset_multi_var1) && nzchar(input$dataset_multi_var1)) {
      last_dataset_multi_var1(input$dataset_multi_var1)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_multi_var2, {
    if (!is.null(input$dataset_multi_var2) && nzchar(input$dataset_multi_var2)) {
      last_dataset_multi_var2(input$dataset_multi_var2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_multi_var3, {
    if (!is.null(input$dataset_multi_var3) && nzchar(input$dataset_multi_var3)) {
      last_dataset_multi_var3(input$dataset_multi_var3)
    }
  }, ignoreInit = TRUE)

  lapply(names(mode_button_map), function(id) {
    observeEvent(input[[id]], {
      updateSelectInput(session, "dataset_mode", selected = mode_button_map[[id]])
    }, ignoreInit = TRUE)
  })

  observe({
    req(input$dataset_mode)
    mode_to_button <- c(
      "General Summary" = "descriptive_mode_summary",
      "Single Variable Analysis" = "descriptive_mode_single",
      "Two Variables Analysis" = "descriptive_mode_two",
      "Three Variables Analysis" = "descriptive_mode_three"
    )

    active_id <- unname(mode_to_button[[input$dataset_mode]])
    shinyjs::removeClass(selector = ".descriptive-tab-button", class = "is-active")
    shinyjs::addClass(id = active_id, class = "is-active")
  })

  observe({
    req(state$data_in())

    if (!is.null(input$dataset_var) && nzchar(input$dataset_var) && input$dataset_var %in% names(state$data_in())) {
      if (is.numeric(state$data_in()[[input$dataset_var]])) {
        updateSelectInput(session, "num1", selected = input$dataset_var)
      } else if (is_categorical(state$data_in()[[input$dataset_var]])) {
        updateSelectInput(session, "cat1", selected = input$dataset_var)
      }
    }
  })

  observe({
    req(state$data_in())

    if (!is.null(input$dataset_var1) && !is.null(input$dataset_var2) &&
        input$dataset_var1 %in% names(state$data_in()) && input$dataset_var2 %in% names(state$data_in())) {
      x <- state$data_in()[[input$dataset_var1]]
      y <- state$data_in()[[input$dataset_var2]]

      if (is.numeric(x) && is.numeric(y)) {
        updateSelectInput(session, "num1", selected = input$dataset_var1)
        updateSelectInput(session, "num2", selected = input$dataset_var2)
      } else if (is_categorical(x) && is_categorical(y)) {
        updateSelectInput(session, "cat1", selected = input$dataset_var1)
        updateSelectInput(session, "cat2", selected = input$dataset_var2)
      }
    }
  })

  output$upload_page_ui <- renderUI({
    source_info <- state$data_source_info()
    sample_choices <- c("Choose a sample dataset" = "", state$sample_dataset_choices)

    div(
      class = "upload-page",
      div(
        class = "dataset-page-header",
        div(
          class = "dataset-page-title",
          h2("Dataset Upload"),
          p("Upload your CSV or Excel file here before moving into descriptive analysis and the rest of the studies.")
        ),
        actionButton("upload_back_home", "Back to Home", class = "btn btn-primary")
      ),
      div(
        class = "dataset-layout",
        div(
          class = "content-card dataset-control-card upload-control-card",
          div(class = "card-header", "Upload Dataset"),
          div(
            class = "upload-card-intro",
            div(class = "upload-card-kicker", "Start here"),
            p("Upload a CSV or Excel file, then review a quick preview before moving to analysis.")
          ),
          div(
            class = "file-upload-wrapper upload-page-wrapper",
            tags$label(
              class = "file-upload-label upload-page-label",
              `for` = "file",
              div(class = "upload-icon", icon("cloud-upload-alt")),
              div(class = "upload-badge", "Upload Dataset"),
              div(class = "upload-text", "Drag or browse"),
              div(class = "upload-subtext", "CSV, XLSX, or XLS")
            ),
            fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls")),
            div(id = "file-name-display", class = "file-name-display")
          ),
          div(
            class = "sample-dataset-card",
            div(class = "sample-dataset-header",
                div(class = "upload-card-kicker", "No file handy?"),
                span(class = "sample-dataset-header-copy", "Use one of the built-in datasets.")
            ),
            div(
              class = "sample-dataset-actions",
              selectInput(
                "sample_dataset",
                NULL,
                choices = sample_choices,
                selected = if (!is.null(state$selected_sample_dataset())) state$selected_sample_dataset() else "",
                width = "100%"
              ),
              actionButton(
                "load_sample_dataset",
                "Load Sample",
                class = "btn btn-secondary sample-dataset-button"
              )
            )
          ),
          div(
            class = "upload-checkbox",
            div(
              class = "upload-checkbox-row",
              tags$input(
                id = "stringsAsFactors",
                type = "checkbox",
                class = "upload-checkbox-input",
                checked = "checked"
              ),
              tags$label(
                `for` = "stringsAsFactors",
                class = "upload-checkbox-copy",
                "Treat text as categories"
              )
            )
          ),
          if (!is.null(source_info)) {
            actionButton("upload_go_descriptive", "Open Descriptive Analysis", class = "btn btn-success btn-block")
          }
        ),
        div(
          class = "dataset-results",
          if (is.null(source_info)) {
            div(
              class = "content-card dataset-empty upload-empty-state",
              div(class = "card-header", "Waiting for a dataset"),
              div(class = "upload-empty-kicker", "Nothing uploaded yet"),
              p("Upload your own file or load one of the built-in datasets. The app will detect numeric and categorical variables and unlock the relevant studies.")
              ,
              div(
                class = "upload-empty-grid",
                div(class = "upload-empty-item", h4("Accepted Files"), p("CSV, XLSX, and XLS are supported.")),
                div(class = "upload-empty-item", h4("Built-in Datasets"), p(paste(length(state$sample_dataset_choices), "sample datasets are ready to load."))),
                div(class = "upload-empty-item", h4("Automatic Detection"), p("Numeric and categorical columns are recognized for you.")),
                div(class = "upload-empty-item", h4("Next Step"), p("Once loaded, move straight into descriptive analysis."))
              )
            )
          } else {
            df <- state$data_in()
            div(
              class = "content-card upload-results-card",
              div(class = "card-header", if (identical(source_info$kind, "sample")) "Sample Dataset" else "Uploaded File"),
              div(
                class = "info-card success upload-ready-message",
                HTML("Your dataset is ready. You can stay here to review the preview, or explore it using <strong>Descriptive Analysis</strong>, <strong>Inferential Statistics</strong>, and the regression tabs from the sidebar.")
              ),
              div(
                class = "upload-summary-grid",
                div(
                  class = "upload-stat-card upload-stat-card-file",
                  div(class = "upload-stat-title", if (identical(source_info$kind, "sample")) "Sample Dataset" else "Source File"),
                  div(class = "upload-stat-value upload-stat-filename", source_info$name),
                  div(
                    class = "upload-stat-meta",
                    if (identical(source_info$kind, "sample")) {
                      paste("Loaded", format(state$upload_time(), "%b %d, %I:%M %p"))
                    } else {
                      paste("Uploaded", format(state$upload_time(), "%b %d, %I:%M %p"))
                    }
                  )
                ),
                div(
                  class = "upload-stat-card upload-stat-card-size",
                  div(class = "upload-stat-title", "Dataset Shape"),
                  div(class = "upload-stat-split",
                      div(class = "upload-stat-pair", span(class = "upload-stat-label", "Rows"), span(class = "upload-stat-number", format(nrow(df), big.mark = ","))),
                      div(class = "upload-stat-pair", span(class = "upload-stat-label", "Columns"), span(class = "upload-stat-number", ncol(df)))
                  )
                ),
                div(
                  class = "upload-stat-card upload-stat-card-vars",
                  div(class = "upload-stat-title", "Variables Detected"),
                  div(class = "upload-stat-split",
                      div(class = "upload-stat-pair", span(class = "upload-stat-label", "Numeric"), span(class = "upload-stat-number", length(state$numeric_vars()))),
                      div(class = "upload-stat-pair", span(class = "upload-stat-label", "Categorical"), span(class = "upload-stat-number", length(state$categorical_vars())))
                  )
                )
              ),
              div(
                class = "table-card upload-preview-card",
                h4("Preview"),
                p(class = "upload-preview-note", "Browse the uploaded rows below before moving into descriptive analysis."),
                withSpinner(DTOutput("upload_preview_table", width = "100%"), color = "#6366f1", type = 4)
              )
            )
          }
        )
      )
    )
  })

  output$descriptive_page_ui <- renderUI({
    if (is.null(state$data_source_info())) {
      return(
        div(
          class = "content-card dataset-empty",
          div(class = "card-header", "Descriptive Analysis"),
          p("No dataset is loaded yet. Open Dataset Upload first, then come back here to explore the data."),
          actionButton("upload_from_descriptive", "Go to Dataset Upload", class = "btn btn-primary")
        )
      )
    }

    selected_mode <- if (!is.null(input$dataset_mode)) input$dataset_mode else "General Summary"

    build_mode_tab <- function(id, title, active = FALSE) {
      tab_class <- paste(
        "btn descriptive-tab-button",
        if (active) "is-active" else ""
      )

      actionButton(
        id,
        label = div(class = "descriptive-tab-title", title),
        class = tab_class
      )
    }

    div(
      class = "dataset-page descriptive-page",
      div(
        class = "dataset-page-header",
        div(
          class = "dataset-page-title",
          h2("Descriptive Analysis"),
          p("Use this page to generate a general summary or descriptive analysis for one, two, or three variables.")
        ),
        NULL
      ),
      div(
        class = "descriptive-tab-shell",
        div(
          class = "descriptive-tab-row",
          build_mode_tab(
            "descriptive_mode_summary",
            "General Summary",
            identical(selected_mode, "General Summary")
          ),
          build_mode_tab(
            "descriptive_mode_single",
            "Single Variable",
            identical(selected_mode, "Single Variable Analysis")
          ),
          build_mode_tab(
            "descriptive_mode_two",
            "Two Variables",
            identical(selected_mode, "Two Variables Analysis")
          ),
          build_mode_tab(
            "descriptive_mode_three",
            "Three Variables",
            identical(selected_mode, "Three Variables Analysis")
          )
        )
      ),
      div(
        class = "descriptive-content-shell",
        div(
          class = "descriptive-mode-hidden",
          selectInput(
            "dataset_mode",
            "Choose analysis view",
            choices = c(
              "General Summary",
              "Single Variable Analysis",
              "Two Variables Analysis",
              "Three Variables Analysis"
            ),
            selected = selected_mode
          )
        ),
        div(class = "descriptive-results-shell",
            div(class = "dataset-results descriptive-results", uiOutput("dataset_results_ui"))
        )
      )
    )
  })

  output$descriptive_overview_ui <- renderUI({
    req(state$data_in())
    df <- state$data_in()

    overview_cards <- list(
      list(icon = "\U0001F4CA", label = "TOTAL ROWS", value = format(nrow(df), big.mark = ","), subtitle = "observations"),
      list(icon = "\U0001F4CB", label = "TOTAL COLUMNS", value = ncol(df), subtitle = "variables"),
      list(icon = "\U0001F522", label = "NUMERIC", value = length(state$numeric_vars()), subtitle = "quantitative"),
      list(icon = "\U0001F3F7", label = "CATEGORICAL", value = length(state$categorical_vars()), subtitle = "qualitative")
    )

    div(
      class = "descriptive-overview-section",
      div(
        class = "descriptive-section-head",
        h3("Dataset Overview"),
        div(
          class = "descriptive-section-meta",
          paste(
            "Last Updated:",
            format(state$upload_time(), "%b %d, %Y at %I:%M %p")
          )
        )
      ),
      div(
        class = "descriptive-metric-grid",
        lapply(overview_cards, function(card) {
          div(
            class = "descriptive-metric-card",
            div(class = "descriptive-metric-icon", card$icon),
            div(class = "descriptive-metric-label", card$label),
            div(class = "descriptive-metric-value", card$value),
            div(class = "descriptive-metric-subtitle", card$subtitle)
          )
        })
      )
    )
  })

  output$dataset_mode_controls <- renderUI({
    req(state$data_in(), input$dataset_mode)
    df <- state$data_in()
    supported_single_vars <- names(df)[vapply(df, function(col) is.numeric(col) || is_categorical(col), logical(1))]
    numeric_choices <- state$numeric_vars()
    categorical_choices <- state$categorical_vars()
    supported_two_vars <- c(numeric_choices, categorical_choices)
    selected_single_var <- if (!is.null(input$dataset_var) && nzchar(input$dataset_var)) input$dataset_var else last_dataset_var()
    selected_first_var <- if (!is.null(input$dataset_var1) && nzchar(input$dataset_var1)) input$dataset_var1 else last_dataset_var1()
    selected_second_var <- if (!is.null(input$dataset_var2) && nzchar(input$dataset_var2)) input$dataset_var2 else last_dataset_var2()
    selected_multi_var1 <- if (!is.null(input$dataset_multi_var1) && nzchar(input$dataset_multi_var1)) input$dataset_multi_var1 else last_dataset_multi_var1()
    selected_multi_var2 <- if (!is.null(input$dataset_multi_var2) && nzchar(input$dataset_multi_var2)) input$dataset_multi_var2 else last_dataset_multi_var2()
    selected_multi_var3 <- if (!is.null(input$dataset_multi_var3) && nzchar(input$dataset_multi_var3)) input$dataset_multi_var3 else last_dataset_multi_var3()
    default_single_var <- if (!is.null(selected_single_var) && selected_single_var %in% supported_single_vars) {
      selected_single_var
    } else if (length(numeric_choices) > 0) {
      numeric_choices[1]
    } else if (length(supported_single_vars) > 0) {
      supported_single_vars[1]
    } else {
      NULL
    }
    default_first_var <- if (!is.null(selected_first_var) && selected_first_var %in% supported_two_vars) {
      selected_first_var
    } else if (length(supported_two_vars) > 0) {
      supported_two_vars[1]
    } else {
      NULL
    }
    selected_quant_var <- if (!is.null(default_single_var) && default_single_var %in% names(df)) default_single_var else NULL
    selected_quant_numeric <- !is.null(selected_quant_var) && is.numeric(df[[selected_quant_var]])
    second_var_choices <- if (!is.null(default_first_var)) {
      if (is.numeric(df[[default_first_var]])) {
        setdiff(numeric_choices, default_first_var)
      } else if (is_categorical(df[[default_first_var]])) {
        setdiff(categorical_choices, default_first_var)
      } else {
        character(0)
      }
    } else {
      character(0)
    }
    default_second_var <- if (!is.null(selected_second_var) && selected_second_var %in% second_var_choices) {
      selected_second_var
    } else if (length(second_var_choices) > 0) {
      second_var_choices[1]
    } else {
      NULL
    }

    multi_defaults <- head(numeric_choices, 3)
    default_multi_var1 <- if (!is.null(selected_multi_var1) && selected_multi_var1 %in% numeric_choices) {
      selected_multi_var1
    } else if (length(multi_defaults) >= 1) {
      multi_defaults[1]
    } else {
      NULL
    }

    default_multi_var2 <- if (!is.null(selected_multi_var2) && selected_multi_var2 %in% numeric_choices) {
      selected_multi_var2
    } else if (length(multi_defaults) >= 2) {
      multi_defaults[2]
    } else {
      NULL
    }

    default_multi_var3 <- if (!is.null(selected_multi_var3) && selected_multi_var3 %in% numeric_choices) {
      selected_multi_var3
    } else if (length(multi_defaults) >= 3) {
      multi_defaults[3]
    } else {
      NULL
    }

    switch(
      input$dataset_mode,
      "General Summary" = tagList(
        div(
          class = "descriptive-filter-block descriptive-filter-block-note",
          div(class = "descriptive-inline-hint", "General Summary uses the full dataset automatically. Scroll down to review the overview, quick stats, and preview table.")
        )
      ),
      "Single Variable Analysis" = tagList(
        div(
          class = "descriptive-filter-block",
          if (length(supported_single_vars) == 0) {
            div(class = "descriptive-inline-hint", "No supported numeric or categorical variables are available for single-variable analysis.")
          } else {
            selectInput(
              "dataset_var",
              "Select a variable",
              choices = supported_single_vars,
              selected = default_single_var
            )
          }
        ),
        if (selected_quant_numeric) {
          div(
            class = "descriptive-filter-block",
            selectInput(
              "dataset_quantile_toggle",
              "Calculate quantile",
              choices = c("No", "Yes"),
              selected = if (!is.null(isolate(input$dataset_quantile_toggle))) isolate(input$dataset_quantile_toggle) else "No"
            )
          )
        },
        if (!is.null(input$dataset_quantile_toggle) && identical(input$dataset_quantile_toggle, "Yes") && selected_quant_numeric) {
          tagList(
            div(
              class = "descriptive-filter-block",
              numericInput(
                "dataset_quantile_prob",
                "Quantile (p-1)",
                value = if (!is.null(isolate(input$dataset_quantile_prob))) as.numeric(isolate(input$dataset_quantile_prob)) else 0.5,
                min = 0,
                max = 1,
                step = 0.05
              )
            ),
            div(
              class = "descriptive-filter-block",
              selectInput(
                "dataset_quantile_type",
                "Quantile type",
                choices = setNames(as.character(1:9), paste("Type", 1:9)),
                selected = if (!is.null(isolate(input$dataset_quantile_type))) isolate(input$dataset_quantile_type) else "7"
              )
            )
          )
        },
        if (selected_quant_numeric) {
          div(
            class = "descriptive-filter-block",
            checkboxInput(
              "dataset_calc_iqr",
              "Calculate IQR",
              value = isTRUE(isolate(input$dataset_calc_iqr))
            )
          )
        }
      ),
      "Two Variables Analysis" = tagList(
        div(
          class = "descriptive-filter-block",
          if (length(supported_two_vars) < 2) {
            div(class = "descriptive-inline-hint", "At least two supported variables are needed for two-variable analysis.")
          } else {
            selectInput(
              "dataset_var1",
              "First variable",
              choices = supported_two_vars,
              selected = default_first_var
            )
          }
        ),
        div(
          class = "descriptive-filter-block",
          if (length(second_var_choices) == 0) {
            div(class = "descriptive-inline-hint", "Choose a supported first variable to see compatible second-variable options.")
          } else {
            selectInput(
              "dataset_var2",
              "Second variable",
              choices = second_var_choices,
              selected = default_second_var
            )
          }
        )
      ),
      "Three Variables Analysis" = tagList(
        div(
          class = "descriptive-filter-block descriptive-filter-block-note",
          p(class = "descriptive-plain-note", "Choose three numeric variables to build a correlation view.")
        ),
        div(
          class = "descriptive-filter-row-three",
          div(
            class = "descriptive-filter-block",
            selectInput("dataset_multi_var1", "First variable", choices = numeric_choices, selected = default_multi_var1)
          ),
          div(
            class = "descriptive-filter-block",
            selectInput("dataset_multi_var2", "Second variable", choices = numeric_choices, selected = default_multi_var2)
          ),
          div(
            class = "descriptive-filter-block",
            selectInput("dataset_multi_var3", "Third variable", choices = numeric_choices, selected = default_multi_var3)
          )
        )
      )
    )
  })

  output$dataset_quantile_ui <- renderUI({
    req(state$data_in(), input$dataset_var, input$dataset_quantile_toggle)
    if (!identical(input$dataset_quantile_toggle, "Yes")) {
      return(NULL)
    }

    df <- state$data_in()
    validate(need(input$dataset_var %in% names(df), "Select a valid variable"))
    x <- df[[input$dataset_var]]
    validate(need(is.numeric(x), "Quantiles are available for numeric variables"))

    prob <- if (!is.null(input$dataset_quantile_prob)) as.numeric(input$dataset_quantile_prob) else 0.5
    prob <- min(max(prob, 0), 1)
    quant_type <- if (!is.null(input$dataset_quantile_type)) as.integer(input$dataset_quantile_type) else 7
    quant_value <- quantile(x, probs = prob, na.rm = TRUE, type = quant_type)

    div(
      class = "info-card success",
      HTML(paste0(
        "<strong>Requested Quantile:</strong> p = ", round(prob, 2), "<br>",
        "<strong>Type:</strong> ", quant_type, "<br>",
        "<strong>Value:</strong> ", round(unname(quant_value), 4)
      ))
    )
  })

  output$descriptive_iqr_ui <- renderUI({
    req(state$data_in(), input$dataset_var)
    if (!isTRUE(input$dataset_calc_iqr)) {
      return(NULL)
    }

    x <- state$data_in()[[input$dataset_var]]
    validate(need(is.numeric(x), "IQR is available for numeric variables"))

    div(
      class = "info-card",
      HTML(paste0(
        "<strong>IQR:</strong> ", round(IQR(x, na.rm = TRUE), 4), "<br>",
        "<strong>Q1:</strong> ", round(quantile(x, 0.25, na.rm = TRUE), 4), "<br>",
        "<strong>Q3:</strong> ", round(quantile(x, 0.75, na.rm = TRUE), 4)
      ))
    )
  })

  output$descriptive_num_stats <- renderUI({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    build_stat_card <- function(title, rows) {
      div(
        class = "visual-stat-item",
        h4(title),
        div(
          class = "kv",
          lapply(rows, function(r) {
            tagList(div(class = "k", r[[1]]), div(class = "v", r[[2]]))
          })
        )
      )
    }

    stat_cards <- list(
      build_stat_card(
        "Central Tendency",
        list(
          c("Mean", round(mean(x, na.rm = TRUE), 3)),
          c("Median", round(median(x, na.rm = TRUE), 3)),
          c("Mode", get_mode(x))
        )
      ),
      build_stat_card(
        "Spread",
        list(
          c("Std Dev", round(sd(x, na.rm = TRUE), 3)),
          c("Variance", round(var(x, na.rm = TRUE), 3)),
          c("Range", round(diff(range(x, na.rm = TRUE)), 3))
        )
      ),
      build_stat_card(
        "Quartiles",
        list(
          c("Q1", round(quantile(x, 0.25, na.rm = TRUE), 3)),
          c("Q2 (Median)", round(quantile(x, 0.5, na.rm = TRUE), 3)),
          c("Q3", round(quantile(x, 0.75, na.rm = TRUE), 3))
        )
      ),
      build_stat_card(
        "Shape",
        list(
          c("Skewness", round(moments::skewness(x, na.rm = TRUE), 3)),
          c("Kurtosis", round(moments::kurtosis(x, na.rm = TRUE), 3))
        )
      )
    )

    if (identical(input$dataset_quantile_toggle, "Yes")) {
      prob <- if (!is.null(input$dataset_quantile_prob)) as.numeric(input$dataset_quantile_prob) else 0.5
      prob <- min(max(prob, 0), 1)
      quant_type <- if (!is.null(input$dataset_quantile_type)) as.integer(input$dataset_quantile_type) else 7
      quant_value <- quantile(x, probs = prob, na.rm = TRUE, type = quant_type)
      stat_cards <- append(
        stat_cards,
        list(
          build_stat_card(
            "Quantile",
            list(
              c("p", round(prob, 2)),
              c("Type", quant_type),
              c("Value", round(unname(quant_value), 4))
            )
          )
        )
      )
    }

    if (isTRUE(input$dataset_calc_iqr)) {
      stat_cards <- append(
        stat_cards,
        list(
          build_stat_card(
            "IQR",
            list(
              c("IQR", round(IQR(x, na.rm = TRUE), 4)),
              c("Q1", round(quantile(x, 0.25, na.rm = TRUE), 4)),
              c("Q3", round(quantile(x, 0.75, na.rm = TRUE), 4))
            )
          )
        )
      )
    }

    card_count <- length(stat_cards)

    div(
      class = paste("visual-stats descriptive-num-stats-grid", paste0("stats-count-", card_count)),
      stat_cards
    )
  })

  output$descriptive_hist <- renderPlot({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    hist(
      x,
      main = paste("Distribution of", input$dataset_var),
      col = "#6366f1",
      xlab = input$dataset_var,
      ylab = "Frequency",
      border = "white",
      breaks = 30,
      las = 1,
      cex.main = 1.4,
      cex.lab = 1.2,
      col.main = "#132a23",
      col.lab = "#374151"
    )
    grid(col = "white", lty = 1, lwd = 1.5)
  })

  output$descriptive_box <- renderPlot({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    boxplot(
      x,
      main = paste("Boxplot of", input$dataset_var),
      horizontal = TRUE,
      col = "#38bdf8",
      border = "#1f2937",
      xlab = input$dataset_var,
      las = 1,
      cex.main = 1.4,
      cex.lab = 1.2,
      col.main = "#132a23",
      col.lab = "#374151",
      lwd = 2
    )
    grid(col = "gray85", lty = 1)
  })

  output$descriptive_cat_stats <- renderUI({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Summary"),
        div(
          class = "kv",
          div(class = "k", "Categories"), div(class = "v", length(freq)),
          div(class = "k", "Most Common"), div(class = "v", names(which.max(freq))),
          div(class = "k", "Max Count"), div(class = "v", max(freq))
        )
      )
    )
  })

  output$descriptive_single_freq_table <- renderDT({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- as.data.frame(table(x, useNA = "ifany"), stringsAsFactors = FALSE)
    names(freq) <- c("Category", "Frequency")

    DT::datatable(freq, options = list(dom = "t", pageLength = 10), rownames = FALSE, class = "cell-border stripe hover")
  })

  output$descriptive_single_prop_table <- renderDT({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")
    prop <- round(prop.table(freq), 4)
    prop_df <- data.frame(Category = names(prop), Proportion = as.numeric(prop), stringsAsFactors = FALSE)

    DT::datatable(prop_df, options = list(dom = "t", pageLength = 10), rownames = FALSE, class = "cell-border stripe hover")
  })

  output$descriptive_bar <- renderPlot({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")
    cols <- rainbow(length(freq))

    barplot(freq, main = paste("Distribution of", input$dataset_var), col = cols, border = "white", las = 2, cex.names = 0.9, cex.main = 1.4, cex.lab = 1.2, col.main = "#132a23", ylab = "Frequency")
    grid(col = "white", lty = 1, lwd = 1.5)
  })

  output$descriptive_pie <- renderPlot({
    req(state$data_in(), input$dataset_var)
    x <- state$data_in()[[input$dataset_var]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")
    prop <- prop.table(freq)
    cols <- rainbow(length(freq))

    pie(freq, main = paste("Pie Chart:", input$dataset_var), col = cols, labels = paste(names(freq), "\n", round(prop * 100, 1), "%"), cex.main = 1.4, col.main = "#132a23", border = "white", cex = 1.1)
  })

  output$descriptive_two_num_stats <- renderUI({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    df <- state$data_in()
    x <- df[[input$dataset_var1]]
    y <- df[[input$dataset_var2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both variables must be numeric"))

    pear <- cor(x, y, method = "pearson", use = "complete.obs")
    spear <- cor(x, y, method = "spearman", use = "complete.obs")

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Pearson Correlation"),
        div(
          class = "kv",
          div(class = "k", "r"), div(class = "v", round(pear, 4)),
          div(class = "k", "Strength"), div(class = "v", if (abs(pear) > 0.7) "Strong" else if (abs(pear) > 0.4) "Moderate" else "Weak")
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Spearman Correlation"),
        div(
          class = "kv",
          div(class = "k", "rho"), div(class = "v", round(spear, 4)),
          div(class = "k", "Type"), div(class = "v", "Rank-based")
        )
      )
    )
  })

  output$descriptive_scatter <- renderPlot({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    df <- state$data_in()
    x <- df[[input$dataset_var1]]
    y <- df[[input$dataset_var2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both variables must be numeric"))

    plot(
      x, y,
      main = paste(input$dataset_var1, "vs", input$dataset_var2),
      xlab = input$dataset_var1,
      ylab = input$dataset_var2,
      pch = 19,
      col = adjustcolor("#6366f1", alpha.f = 0.72),
      cex = 1.4,
      cex.main = 1.5,
      cex.lab = 1.2,
      col.main = "#132a23",
      col.lab = "#374151",
      las = 1
    )
    abline(lm(y ~ x), col = "#ef4444", lwd = 3)
    grid(col = "gray85", lty = 1)
  })

  output$descriptive_freq_table <- renderDT({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    df <- state$data_in()
    a <- df[[input$dataset_var1]]
    b <- df[[input$dataset_var2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both variables must be categorical"))

    tab <- table(a, b, useNA = "ifany")
    DT::datatable(as.data.frame.matrix(tab), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$descriptive_prop_table <- renderDT({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    df <- state$data_in()
    a <- df[[input$dataset_var1]]
    b <- df[[input$dataset_var2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both variables must be categorical"))

    tab <- table(a, b, useNA = "ifany")
    prop_tab <- prop.table(tab)
    DT::datatable(round(as.data.frame.matrix(prop_tab), 4), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$descriptive_stacked <- renderPlot({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    tab <- table(state$data_in()[[input$dataset_var1]], state$data_in()[[input$dataset_var2]], useNA = "ifany")
    cols <- c("#6366f1", "#1d4ed8", "#ec4899", "#10b981", "#f97316")
    barplot(tab, main = "Stacked Bar Plot", col = cols[1:nrow(tab)], border = "white", las = 1, cex.main = 1.3, col.main = "#132a23")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })

  output$descriptive_side <- renderPlot({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    tab <- table(state$data_in()[[input$dataset_var1]], state$data_in()[[input$dataset_var2]], useNA = "ifany")
    cols <- c("#6366f1", "#1d4ed8", "#ec4899", "#10b981", "#f97316")
    barplot(tab, beside = TRUE, main = "Side-by-Side Bar Plot", col = cols[1:nrow(tab)], border = "white", las = 1, cex.main = 1.3, col.main = "#132a23")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })

  output$descriptive_mosaic <- renderPlot({
    req(state$data_in(), input$dataset_var1, input$dataset_var2)
    tab <- table(state$data_in()[[input$dataset_var1]], state$data_in()[[input$dataset_var2]], useNA = "ifany")
    mosaicplot(tab, main = "Mosaic Plot", color = c("#6366f1", "#1d4ed8", "#ec4899", "#10b981"), cex.axis = 1, col.main = "#132a23", cex.main = 1.3)
  })

  output$dataset_results_ui <- renderUI({
    req(state$data_in(), input$dataset_mode)
    df <- state$data_in()
    supported_single_vars <- names(df)[vapply(df, function(col) is.numeric(col) || is_categorical(col), logical(1))]
    numeric_choices <- state$numeric_vars()
    categorical_choices <- state$categorical_vars()
    supported_two_vars <- c(numeric_choices, categorical_choices)

    switch(
      input$dataset_mode,
      "General Summary" = tagList(
        div(
          class = "descriptive-tab-content",
          uiOutput("descriptive_overview_ui"),
          uiOutput("dashboard_ui"),
          div(
            class = "content-card",
            div(class = "card-header", "Dataset Preview"),
            div(class = "table-card", DTOutput("table"))
          )
        )
      ),
      "Single Variable Analysis" = {
        validate(need(length(supported_single_vars) > 0, "No supported numeric or categorical variables are available for single-variable analysis."))

        # Use a safe default on first render so the controls can appear before the
        # selectInput binding exists. Once the user chooses a value, Shiny will use it.
        active_var <- if (!is.null(input$dataset_var) && input$dataset_var %in% supported_single_vars) {
          input$dataset_var
        } else {
          supported_single_vars[1]
        }
        x <- df[[active_var]]

        if (is.numeric(x)) {
          div(
            class = "descriptive-tab-content",
            div(
              class = "content-card descriptive-controls-panel",
              div(class = "card-header", "Variable Selection"),
              p(class = "descriptive-panel-note", "Select one numeric variable to inspect its distribution, shape, and optional quantile metrics."),
              div(class = "descriptive-filter-row", uiOutput("dataset_mode_controls"))
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Summary Statistics"),
              uiOutput("descriptive_num_stats")
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Visualizations"),
              div(
                class = "plot-row",
                withSpinner(plotOutput("descriptive_hist", height = "400px"), color = "#6366f1", type = 4),
                withSpinner(plotOutput("descriptive_box", height = "400px"), color = "#6366f1", type = 4)
              )
            )
          )
        } else if (is_categorical(x)) {
          div(
            class = "descriptive-tab-content",
            div(
              class = "content-card descriptive-controls-panel",
              div(class = "card-header", "Variable Selection"),
              p(class = "descriptive-panel-note", "Select one categorical variable to review counts, proportions, and category distributions."),
              div(class = "descriptive-filter-row", uiOutput("dataset_mode_controls"))
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Summary Statistics"),
              uiOutput("descriptive_cat_stats"),
              div(
                class = "plot-row",
                div(class = "table-card", h4("Frequency Table"), withSpinner(DTOutput("descriptive_single_freq_table"), color = "#6366f1", type = 4)),
                div(class = "table-card", h4("Proportion Table"), withSpinner(DTOutput("descriptive_single_prop_table"), color = "#6366f1", type = 4))
              )
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Visualizations"),
              div(
                class = "plot-row",
                withSpinner(plotOutput("descriptive_bar", height = "420px"), color = "#6366f1", type = 4),
                withSpinner(plotOutput("descriptive_pie", height = "420px"), color = "#6366f1", type = 4)
              )
            )
          )
        } else {
          div(class = "info-card warning", "This variable type is not supported for descriptive analysis on this page.")
        }
      },
      "Two Variables Analysis" = {
        validate(need(length(supported_two_vars) >= 2, "At least two supported variables are needed for two-variable analysis."))

        active_var1 <- if (!is.null(input$dataset_var1) && input$dataset_var1 %in% supported_two_vars) {
          input$dataset_var1
        } else {
          supported_two_vars[1]
        }

        compatible_var2_choices <- if (is.numeric(df[[active_var1]])) {
          setdiff(numeric_choices, active_var1)
        } else if (is_categorical(df[[active_var1]])) {
          setdiff(categorical_choices, active_var1)
        } else {
          character(0)
        }

        validate(need(length(compatible_var2_choices) > 0, "Choose a supported first variable to see compatible second-variable options."))

        active_var2 <- if (!is.null(input$dataset_var2) && input$dataset_var2 %in% compatible_var2_choices) {
          input$dataset_var2
        } else {
          compatible_var2_choices[1]
        }

        x <- df[[active_var1]]
        y <- df[[active_var2]]

        if (is.numeric(x) && is.numeric(y)) {
          div(
            class = "descriptive-tab-content",
            div(
              class = "content-card descriptive-controls-panel",
              div(class = "card-header", "Variable Selection"),
              p(class = "descriptive-panel-note", "Choose two numeric variables to compare their relationship with correlation measures and a scatter plot."),
              div(class = "descriptive-filter-row descriptive-filter-row-two", uiOutput("dataset_mode_controls"))
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Relationship Summary"),
              uiOutput("descriptive_two_num_stats")
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Scatter Plot"),
              withSpinner(plotOutput("descriptive_scatter", height = "520px"), color = "#6366f1", type = 4)
            )
          )
        } else if (is_categorical(x) && is_categorical(y)) {
          div(
            class = "descriptive-tab-content",
            div(
              class = "content-card descriptive-controls-panel",
              div(class = "card-header", "Variable Selection"),
              p(class = "descriptive-panel-note", "Choose two categorical variables to compare frequencies, proportions, and side-by-side plots."),
              div(class = "descriptive-filter-row descriptive-filter-row-two", uiOutput("dataset_mode_controls"))
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Contingency Tables"),
              div(class = "table-card", h4("Frequency Table"), withSpinner(DTOutput("descriptive_freq_table"), color = "#6366f1", type = 4)),
              div(class = "table-card", h4("Proportions"), withSpinner(DTOutput("descriptive_prop_table"), color = "#6366f1", type = 4))
            ),
            div(
              class = "content-card",
              div(class = "card-header", "Comparative Plots"),
              fluidRow(
                column(4, withSpinner(plotOutput("descriptive_stacked", height = "420px"), color = "#6366f1", type = 4)),
                column(4, withSpinner(plotOutput("descriptive_side", height = "420px"), color = "#6366f1", type = 4)),
                column(4, withSpinner(plotOutput("descriptive_mosaic", height = "420px"), color = "#6366f1", type = 4))
              )
            )
          )
        } else {
          div(
            class = "info-card warning",
            "Choose either two numeric variables or two categorical variables for descriptive analysis. For mixed types, you can continue later with dedicated study pages."
          )
        }
      },
      "Three Variables Analysis" = div(
        class = "descriptive-tab-content",
        div(
          class = "content-card descriptive-controls-panel",
          div(class = "card-header", "Variable Selection"),
          div(uiOutput("dataset_mode_controls"))
        ),
        div(
          class = "content-card",
          div(class = "card-header", "Correlation Table"),
          withSpinner(DTOutput("dataset_corr_tbl"), color = "#6366f1", type = 4)
        ),
        div(
          class = "content-card",
          div(class = "card-header", "Multi-Variable Visualizations"),
          div(
            class = "plot-row",
            withSpinner(plotOutput("dataset_pairs_plot", height = "500px"), color = "#6366f1", type = 4),
            withSpinner(plotOutput("dataset_corr_plot", height = "500px"), color = "#6366f1", type = 4)
          )
        )
      )
    )
  })

  output$data_summary_cards <- renderUI({
    df <- state$data_in()

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Dataset Size"),
        div(
          class = "kv",
          div(class = "k", "Rows"), div(class = "v", format(nrow(df), big.mark = ",")),
          div(class = "k", "Columns"), div(class = "v", ncol(df))
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Variable Types"),
        div(
          class = "kv",
          div(class = "k", "Numeric"), div(class = "v", length(state$numeric_vars())),
          div(class = "k", "Categorical"), div(class = "v", length(state$categorical_vars()))
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Data Quality"),
        div(
          class = "kv",
          div(class = "k", "Complete"), div(class = "v", paste0(round((1 - sum(is.na(df)) / (nrow(df) * ncol(df))) * 100, 1), "%")),
          div(class = "k", "Missing"), div(class = "v", sum(is.na(df)))
        )
      )
    )
  })

  output$table <- renderDT({
    df <- state$data_in()
    DT::datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE),
      class = "cell-border stripe hover",
      rownames = FALSE
    )
  })

  output$upload_preview_table <- renderDT({
    df <- state$data_in()
    DT::datatable(
      df,
      options = list(
        dom = "tip",
        pageLength = 6,
        lengthChange = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE
      ),
      class = "cell-border stripe hover compact",
      rownames = FALSE
    )
  })

  output$dataset_corr_tbl <- renderDT({
    req(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    validate(need(all(c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3) %in% names(state$data_in())), "Select valid numeric variables"))
    vars <- c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    nums <- state$data_in()[, vars, drop = FALSE]
    cm <- cor(nums, use = "complete.obs")
    DT::datatable(round(cm, 3), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$dataset_pairs_plot <- renderPlot({
    req(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    validate(need(all(c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3) %in% names(state$data_in())), "Select valid numeric variables"))
    vars <- c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    nums <- state$data_in()[, vars, drop = FALSE]
    pairs(nums, main = "Selected Numeric Variables", pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.55), cex = 0.8, col.main = "#132a23", cex.main = 1.4)
  })

  output$dataset_corr_plot <- renderPlot({
    req(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    validate(need(all(c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3) %in% names(state$data_in())), "Select valid numeric variables"))
    vars <- c(input$dataset_multi_var1, input$dataset_multi_var2, input$dataset_multi_var3)
    nums <- state$data_in()[, vars, drop = FALSE]
    cm <- cor(nums, use = "complete.obs")
    corrplot(cm, method = "circle", type = "full", diag = FALSE, col = colorRampPalette(c("#ef4444", "white", "#6366f1"))(100), tl.col = "#132a23", tl.cex = 1)
  })
}
