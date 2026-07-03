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
    home_nav_descriptive = "tab_descriptive",
    home_nav_tests = "tab_tests",
    home_nav_regression = "tab_regression",
    home_nav_adequacy = "tab_adequacy",
    home_nav_corrections = "tab_corrections",
    home_nav_multicollinearity = "tab_multicollinearity",
    home_nav_model_building = "tab_model_building",
    home_nav_influence = "tab_influence",
    home_nav_polynomial = "tab_polynomial",
    home_nav_spline = "tab_spline",
    home_nav_glm = "tab_glm",
    card_tests = "tab_tests",
    card_adequacy = "tab_adequacy",
    card_corrections = "tab_corrections",
    card_multicollinearity = "tab_multicollinearity",
    card_model_building = "tab_model_building"
  )

  lapply(names(navigation_map), function(id) {
    observeEvent(input[[id]], {
      state$current_tab(navigation_map[[id]])
      shinyjs::runjs("document.getElementById('app-shell').classList.remove('sidebar-open');")
    }, ignoreInit = TRUE)
  })

  observeEvent(input$toggle_sidebar, {
    shinyjs::runjs(
      "var shell = document.getElementById('app-shell');
       var isMobile = window.innerWidth <= 1100;
       if (isMobile) {
         shell.classList.toggle('sidebar-open');
       } else {
         shell.classList.toggle('sidebar-collapsed');
       }
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
    enable_targets(c("home_nav_descriptive", "home_nav_tests", "home_nav_regression", "home_nav_adequacy", "home_nav_corrections", "home_nav_multicollinearity", "home_nav_model_building", "home_nav_influence", "home_nav_polynomial", "home_nav_spline", "home_nav_glm"), TRUE)
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
