register_analysis_server <- function(input, output, session, state) {
  tests_ready <- reactiveVal(FALSE)

  # Controls which inferential "scope" tab is active (one vs two vs three+ samples).
  infer_sample_scope <- reactiveVal("One Population Sample")

  observeEvent(input$tests_analyze, {
    tests_ready(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$infer_scope_one, {
    infer_sample_scope("One Population Sample")
    tests_ready(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$infer_scope_two, {
    infer_sample_scope("Two Population Sample")
    tests_ready(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$infer_scope_three, {
    infer_sample_scope("Three or more Population Sample")
    tests_ready(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(state$data_in(), {
    tests_ready(FALSE)
    infer_sample_scope("One Population Sample")
  }, ignoreInit = TRUE)

  output$num_stats_cards <- renderUI({
    req(input$num1)
    x <- state$data_in()[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Central Tendency"),
        div(
          class = "kv",
          div(class = "k", "Mean"), div(class = "v", round(mean(x, na.rm = TRUE), 3)),
          div(class = "k", "Median"), div(class = "v", round(median(x, na.rm = TRUE), 3)),
          div(class = "k", "Mode"), div(class = "v", get_mode(x))
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Spread"),
        div(
          class = "kv",
          div(class = "k", "Std Dev"), div(class = "v", round(sd(x, na.rm = TRUE), 3)),
          div(class = "k", "Variance"), div(class = "v", round(var(x, na.rm = TRUE), 3)),
          div(class = "k", "Range"), div(class = "v", round(diff(range(x, na.rm = TRUE)), 3))
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Quartiles"),
        div(
          class = "kv",
          div(class = "k", "Q1"), div(class = "v", round(quantile(x, 0.25, na.rm = TRUE), 3)),
          div(class = "k", "Q2 (Median)"), div(class = "v", round(quantile(x, 0.5, na.rm = TRUE), 3)),
          div(class = "k", "Q3"), div(class = "v", round(quantile(x, 0.75, na.rm = TRUE), 3))
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Shape"),
        div(
          class = "kv",
          div(class = "k", "Skewness"), div(class = "v", round(moments::skewness(x, na.rm = TRUE), 3)),
          div(class = "k", "Kurtosis"), div(class = "v", round(moments::kurtosis(x, na.rm = TRUE), 3)),
          div(class = "k", "IQR"), div(class = "v", round(IQR(x, na.rm = TRUE), 3))
        )
      )
    )
  })

  output$hist <- renderPlot({
    req(input$num1)
    x <- state$data_in()[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    hist(
      x,
      main = paste("Distribution of", input$num1),
      col = "#6366f1",
      xlab = input$num1,
      ylab = "Frequency",
      border = "white",
      breaks = 30,
      las = 1,
      cex.main = 1.4,
      cex.lab = 1.2,
      col.main = "#111827",
      col.lab = "#374151"
    )
    grid(col = "white", lty = 1, lwd = 1.5)
  })

  output$box <- renderPlot({
    req(input$num1)
    x <- state$data_in()[[input$num1]]
    validate(need(is.numeric(x), "Select a numeric variable"))

    boxplot(
      x,
      main = paste("Boxplot of", input$num1),
      horizontal = TRUE,
      col = "#a855f7",
      border = "#1f2937",
      xlab = input$num1,
      las = 1,
      cex.main = 1.4,
      cex.lab = 1.2,
      col.main = "#111827",
      col.lab = "#374151",
      lwd = 2
    )
    grid(col = "gray85", lty = 1)
  })

  output$download_num_stats <- downloadHandler(
    filename = function() paste0("stats_", input$num1, "_", Sys.Date(), ".txt"),
    content = function(file) {
      x <- state$data_in()[[input$num1]]
      sink(file)
      cat("Statistics for:", input$num1, "\n\n")
      cat("Mean:", mean(x, na.rm = TRUE), "\n")
      cat("Median:", median(x, na.rm = TRUE), "\n")
      cat("SD:", sd(x, na.rm = TRUE), "\n")
      sink()
    }
  )

  output$download_hist <- downloadHandler(
    filename = function() paste0("histogram_", input$num1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      x <- state$data_in()[[input$num1]]
      hist(x, main = paste("Distribution of", input$num1), col = "#6366f1", border = "white", breaks = 30)
      dev.off()
    }
  )

  output$download_box <- downloadHandler(
    filename = function() paste0("boxplot_", input$num1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      x <- state$data_in()[[input$num1]]
      boxplot(x, main = paste("Boxplot of", input$num1), horizontal = TRUE, col = "#a855f7")
      dev.off()
    }
  )

  output$two_num_stats_cards <- renderUI({
    req(input$num1, input$num2)
    df <- state$data_in()
    x <- df[[input$num1]]
    y <- df[[input$num2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both must be numeric"))

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
          div(class = "k", "ρ"), div(class = "v", round(spear, 4)),
          div(class = "k", "Type"), div(class = "v", "Rank-based")
        )
      )
    )
  })

  output$scatter <- renderPlot({
    req(input$num1, input$num2)
    df <- state$data_in()
    x <- df[[input$num1]]
    y <- df[[input$num2]]
    validate(need(is.numeric(x) && is.numeric(y), "Both must be numeric"))

    plot(
      x, y,
      main = paste(input$num1, "vs", input$num2),
      xlab = input$num1,
      ylab = input$num2,
      pch = 19,
      col = adjustcolor("#6366f1", alpha.f = 0.7),
      cex = 1.5,
      cex.main = 1.5,
      cex.lab = 1.2,
      col.main = "#111827",
      col.lab = "#374151",
      las = 1
    )

    abline(lm(y ~ x), col = "#ef4444", lwd = 3)
    grid(col = "gray85", lty = 1)
  })

  output$download_scatter <- downloadHandler(
    filename = function() paste0("scatter_", input$num1, "_vs_", input$num2, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 900, res = 120)
      x <- state$data_in()[[input$num1]]
      y <- state$data_in()[[input$num2]]
      plot(x, y, pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.7), cex = 1.5)
      abline(lm(y ~ x), col = "#ef4444", lwd = 3)
      dev.off()
    }
  )

  output$corr_tbl <- renderDT({
    nums <- state$data_in()[, state$numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    cm <- cor(nums, use = "complete.obs")
    DT::datatable(round(cm, 3), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$pairs_plot <- renderPlot({
    nums <- state$data_in()[, state$numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    pairs(nums, main = "Scatterplot Matrix", pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.5), cex = 0.8, col.main = "#111827", cex.main = 1.4)
  })

  output$corr_plot <- renderPlot({
    nums <- state$data_in()[, state$numeric_vars(), drop = FALSE]
    validate(need(ncol(nums) >= 2, "Need at least 2 numeric columns"))
    cm <- cor(nums, use = "complete.obs")
    corrplot(cm, method = "circle", type = "full", diag = FALSE, col = colorRampPalette(c("#ef4444", "white", "#6366f1"))(100), tl.col = "#111827", tl.cex = 1)
  })

  output$download_corr_csv <- downloadHandler(
    filename = function() paste0("correlation_", Sys.Date(), ".csv"),
    content = function(file) {
      cm <- cor(state$data_in()[, state$numeric_vars(), drop = FALSE], use = "complete.obs")
      write.csv(cm, file, row.names = TRUE)
    }
  )

  output$download_pairs <- downloadHandler(
    filename = function() paste0("pairs_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 1200, res = 120)
      pairs(state$data_in()[, state$numeric_vars(), drop = FALSE], pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.5))
      dev.off()
    }
  )

  output$download_corrplot <- downloadHandler(
    filename = function() paste0("corrplot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 1000, res = 120)
      cm <- cor(state$data_in()[, state$numeric_vars(), drop = FALSE], use = "complete.obs")
      corrplot(cm, method = "circle", col = colorRampPalette(c("#ef4444", "white", "#6366f1"))(100))
      dev.off()
    }
  )

  output$cat_stats_cards <- renderUI({
    req(input$cat1)
    x <- state$data_in()[[input$cat1]]
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

  output$bar <- renderPlot({
    req(input$cat1)
    x <- state$data_in()[[input$cat1]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")
    cols <- rainbow(length(freq))

    barplot(freq, main = paste("Distribution of", input$cat1), col = cols, border = "white", las = 2, cex.names = 0.9, cex.main = 1.4, cex.lab = 1.2, col.main = "#111827", ylab = "Frequency")
    grid(col = "white", lty = 1, lwd = 1.5)
  })

  output$pie <- renderPlot({
    req(input$cat1)
    x <- state$data_in()[[input$cat1]]
    validate(need(is_categorical(x), "Select a categorical variable"))

    freq <- table(x, useNA = "ifany")
    prop <- prop.table(freq)
    cols <- rainbow(length(freq))

    pie(freq, main = paste("Pie Chart:", input$cat1), col = cols, labels = paste(names(freq), "\n", round(prop * 100, 1), "%"), cex.main = 1.4, col.main = "#111827", border = "white", cex = 1.1)
  })

  output$download_bar <- downloadHandler(
    filename = function() paste0("bar_", input$cat1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 700, res = 120)
      freq <- table(state$data_in()[[input$cat1]], useNA = "ifany")
      cols <- rainbow(length(freq))
      barplot(freq, col = cols, border = "white")
      dev.off()
    }
  )

  output$download_pie <- downloadHandler(
    filename = function() paste0("pie_", input$cat1, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1000, height = 1000, res = 120)
      freq <- table(state$data_in()[[input$cat1]], useNA = "ifany")
      prop <- prop.table(freq)
      cols <- rainbow(length(freq))
      pie(freq, col = cols, labels = paste(names(freq), "\n", round(prop * 100, 1), "%"))
      dev.off()
    }
  )

  output$freq_table <- renderDT({
    req(input$cat1, input$cat2)
    df <- state$data_in()
    a <- df[[input$cat1]]
    b <- df[[input$cat2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both must be categorical"))

    tab <- table(a, b, useNA = "ifany")
    DT::datatable(as.data.frame.matrix(tab), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$prop_table <- renderDT({
    req(input$cat1, input$cat2)
    df <- state$data_in()
    a <- df[[input$cat1]]
    b <- df[[input$cat2]]
    validate(need(is_categorical(a) && is_categorical(b), "Both must be categorical"))

    tab <- table(a, b, useNA = "ifany")
    prop_tab <- prop.table(tab)
    DT::datatable(round(as.data.frame.matrix(prop_tab), 4), options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover")
  })

  output$download_freq_csv <- downloadHandler(
    filename = function() paste0("freq_", input$cat1, "_", input$cat2, "_", Sys.Date(), ".csv"),
    content = function(file) {
      tab <- table(state$data_in()[[input$cat1]], state$data_in()[[input$cat2]], useNA = "ifany")
      write.csv(as.data.frame.matrix(tab), file, row.names = TRUE)
    }
  )

  output$download_prop_csv <- downloadHandler(
    filename = function() paste0("prop_", input$cat1, "_", input$cat2, "_", Sys.Date(), ".csv"),
    content = function(file) {
      tab <- table(state$data_in()[[input$cat1]], state$data_in()[[input$cat2]], useNA = "ifany")
      write.csv(round(as.data.frame.matrix(prop.table(tab)), 4), file, row.names = TRUE)
    }
  )

  output$stacked <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(state$data_in()[[input$cat1]], state$data_in()[[input$cat2]], useNA = "ifany")
    cols <- c("#6366f1", "#a855f7", "#ec4899", "#10b981", "#f97316")
    barplot(tab, main = "Stacked Bar Plot", col = cols[1:nrow(tab)], border = "white", las = 1, cex.main = 1.3, col.main = "#111827")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })

  output$side <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(state$data_in()[[input$cat1]], state$data_in()[[input$cat2]], useNA = "ifany")
    cols <- c("#6366f1", "#a855f7", "#ec4899", "#10b981", "#f97316")
    barplot(tab, beside = TRUE, main = "Side-by-Side Bar Plot", col = cols[1:nrow(tab)], border = "white", las = 1, cex.main = 1.3, col.main = "#111827")
    legend("topright", legend = rownames(tab), fill = cols[1:nrow(tab)], cex = 0.9)
    grid(col = "white", lty = 1)
  })

  output$mosaic <- renderPlot({
    req(input$cat1, input$cat2)
    tab <- table(state$data_in()[[input$cat1]], state$data_in()[[input$cat2]], useNA = "ifany")
    mosaicplot(tab, main = "Mosaic Plot", color = c("#6366f1", "#a855f7", "#ec4899", "#10b981"), cex.axis = 1, col.main = "#111827", cex.main = 1.3)
  })

  output$tests_out_ui <- renderUI({
    df <- state$data_in()
    numeric_choices <- state$numeric_vars()
    validate(need(length(numeric_choices) > 0, "No numeric variables are available for inferential statistics."))

    analyzed <- isTRUE(tests_ready())

    sample_scope <- infer_sample_scope()

    # ---------- One Population Sample (existing workflow) ----------
    test_type <- if (!is.null(input$infer_test_type)) input$infer_test_type else "Normality Check"
    alpha_one <- if (!is.null(input$tests_alpha)) input$tests_alpha else 0.05
    alpha_one <- max(min(as.numeric(alpha_one), 0.2), 0.001)

    normality_var <- if (!is.null(input$tests_norm_var) && input$tests_norm_var %in% numeric_choices) input$tests_norm_var else numeric_choices[1]
    location_var <- if (!is.null(input$tests_loc_var) && input$tests_loc_var %in% numeric_choices) input$tests_loc_var else numeric_choices[1]
    location_choice <- if (!is.null(input$tests_loc_choice)) input$tests_loc_choice else "All"
    m0_value <- if (!is.null(input$tests_m0)) as.numeric(input$tests_m0) else 0
    alt_value <- if (!is.null(input$tests_alt)) input$tests_alt else "two.sided"

    # ---------- Two Population Samples (new workflow: normality only) ----------
    two_x <- if (!is.null(input$tests_two_varx) && input$tests_two_varx %in% numeric_choices) input$tests_two_varx else numeric_choices[1]
    two_y_default <- if (length(numeric_choices) >= 2) numeric_choices[2] else numeric_choices[1]
    two_y <- if (!is.null(input$tests_two_vary) && input$tests_two_vary %in% numeric_choices) input$tests_two_vary else two_y_default

    alpha_two <- if (!is.null(input$tests_two_alpha)) input$tests_two_alpha else 0.05
    alpha_two <- max(min(as.numeric(alpha_two), 0.2), 0.001)

    test_type_two <- if (!is.null(input$infer_test_type_two)) input$infer_test_type_two else "Normality Check"
    two_m0_value <- if (!is.null(input$tests_two_m0)) as.numeric(input$tests_two_m0) else 0
    two_alt_value <- if (!is.null(input$tests_two_alt)) input$tests_two_alt else "two.sided"
    two_loc_choice <- if (!is.null(input$tests_two_loc_choice)) input$tests_two_loc_choice else "All"

    # ---------- Three or More (new workflow: normality for each selected variable) ----------
    default_three <- head(numeric_choices, 3)
    three_vars <- if (!is.null(input$tests_three_vars) && length(input$tests_three_vars) > 0) {
      input$tests_three_vars[input$tests_three_vars %in% numeric_choices]
    } else {
      default_three
    }
    alpha_three <- if (!is.null(input$tests_three_alpha)) input$tests_three_alpha else 0.05
    alpha_three <- max(min(as.numeric(alpha_three), 0.2), 0.001)

    test_type_three <- if (!is.null(input$infer_test_type_three)) input$infer_test_type_three else "Normality Check"
    three_loc_choice <- if (!is.null(input$tests_three_loc_choice)) input$tests_three_loc_choice else "All"

    # Used only for CSS corner-fill (avoid depending on :has()).
    infer_scope_class <- if (identical(sample_scope, "One Population Sample")) {
      "inferential-scope-one"
    } else if (identical(sample_scope, "Two Population Sample")) {
      "inferential-scope-two"
    } else {
      "inferential-scope-three"
    }

    tagList(
      div(
        class = "tests-page-hero",
        h2("Inferential Statistics"),
        p("Run inferential procedures on numeric variables by selecting a test type, setting analysis parameters, and reviewing interpreted results.")
      ),
      div(
        class = paste("inferential-tab-shell", infer_scope_class),
        div(
          class = "inferential-tab-row",
          actionButton(
            "infer_scope_one",
            span(class = "action-label inferential-tab-title", "One Population Sample"),
            class = paste("btn inferential-tab-button", if (identical(sample_scope, "One Population Sample")) "is-active" else ""),
            style = "width: auto;"
          ),
          actionButton(
            "infer_scope_two",
            span(class = "action-label inferential-tab-title", "Two Population Sample"),
            class = paste("btn inferential-tab-button", if (identical(sample_scope, "Two Population Sample")) "is-active" else ""),
            style = "width: auto;"
          ),
          actionButton(
            "infer_scope_three",
            span(class = "action-label inferential-tab-title", "Three or more Population Sample"),
            class = paste("btn inferential-tab-button", if (identical(sample_scope, "Three or more Population Sample")) "is-active" else ""),
            style = "width: auto;"
          )
        )
      ),
      div(
        class = "inferential-results-shell",
        div(
          class = "tests-panel tests-input-panel",
          div(class = "tests-section-title", "Variable Selection"),
          div(
            # One Population Sample input panel (same as current workflow)
            div(
              class = if (identical(sample_scope, "One Population Sample")) "" else "descriptive-mode-hidden",
              div(
                class = paste(
                  "tests-input-grid",
                  if (identical(test_type, "Location Parameter Test")) "tests-input-grid-location" else "",
                  if (identical(test_type, "Normality Check")) "tests-input-grid-normality" else ""
                ),
                div(
                  class = "tests-control",
                  selectInput(
                    "infer_test_type",
                    "Type of test",
                    choices = c("Normality Check", "Location Parameter Test"),
                    selected = test_type
                  )
                ),
                if (identical(test_type, "Normality Check")) {
                  tagList(
                    div(
                      class = "tests-control",
                      selectInput("tests_norm_var", "Select a numeric variable to test", choices = numeric_choices, selected = normality_var)
                    ),
                    div(
                      class = "tests-control",
                      numericInput("tests_alpha", "Significance level (α)", value = alpha_one, min = 0.001, max = 0.2, step = 0.001)
                    )
                  )
                } else {
                  tagList(
                    div(
                      class = "tests-control",
                      selectInput("tests_loc_var", "Select a numeric variable to test", choices = numeric_choices, selected = location_var)
                    ),
                    div(
                      class = "tests-control",
                      selectInput("tests_loc_choice", "Select the test", choices = c("z test", "t test", "Wilcoxon", "All"), selected = location_choice)
                    ),
                    div(
                      class = "tests-control",
                      numericInput("tests_m0", "Hypothesized median (m₀)", value = m0_value, step = 0.1)
                    ),
                    div(
                      class = "tests-control",
                      numericInput("tests_alpha", "Significance level (α)", value = alpha_one, min = 0.001, max = 0.2, step = 0.001)
                    ),
                    div(
                      class = "tests-control",
                      selectInput(
                        "tests_alt",
                        "Alternative hypothesis",
                        choices = c("two.sided" = "two.sided", "greater" = "greater", "less" = "less"),
                        selected = alt_value
                      )
                    )
                  )
                }
              )
            ),
            # Two Population Samples input panel
            div(
              class = if (identical(sample_scope, "Two Population Sample")) "" else "descriptive-mode-hidden",
              div(
                class = "tests-input-grid tests-input-grid-two",
                div(
                  class = "tests-control",
                  selectInput(
                    "infer_test_type_two",
                    "Select Test Type",
                    choices = c("Normality Check", "Location Parameter Test"),
                    selected = test_type_two
                  )
                ),
                div(
                  class = "tests-control",
                  selectInput("tests_two_varx", "Select Column for Sample X", choices = numeric_choices, selected = two_x)
                ),
                div(
                  class = "tests-control",
                  selectInput("tests_two_vary", "Select Column for Sample Y", choices = numeric_choices, selected = two_y)
                ),
                if (identical(test_type_two, "Location Parameter Test")) {
                  tagList(
                    div(
                      class = "tests-control",
                      numericInput("tests_two_m0", "Hypothesized difference (m₀)", value = two_m0_value, step = 0.1)
                    ),
                    div(
                      class = "tests-control",
                      selectInput(
                        "tests_two_loc_choice",
                        "Select Tests",
                        choices = c(
                          "z test" = "z test",
                          "t test (equal variances)" = "t test equal",
                          "t test (Welch / unequal variances)" = "t test welch",
                          "Wilcoxon (independent)" = "Wilcoxon",
                          "All" = "All"
                        ),
                        selected = two_loc_choice
                      )
                    ),
                    div(
                      class = "tests-control",
                      selectInput(
                        "tests_two_alt",
                        "Alternative hypothesis",
                        choices = c("two.sided" = "two.sided", "greater" = "greater", "less" = "less"),
                        selected = two_alt_value
                      )
                    )
                  )
                },
                div(
                  class = "tests-control",
                  numericInput("tests_two_alpha", "Significance level (α)", value = alpha_two, min = 0.001, max = 0.2, step = 0.001)
                )
              )
            ),
            # Three or More input panel
            div(
              class = if (identical(sample_scope, "Three or more Population Sample")) "" else "descriptive-mode-hidden",
              div(
                class = "tests-input-grid tests-input-grid-three",
                div(
                  class = "tests-control",
                  selectInput(
                    "infer_test_type_three",
                    "Select Test Type",
                    choices = c("Normality Check", "Location Parameter Test"),
                    selected = test_type_three
                  )
                ),
                div(
                  class = "tests-control",
                  selectInput("tests_three_vars", "Select Columns", choices = numeric_choices, selected = three_vars, multiple = TRUE)
                ),
                if (identical(test_type_three, "Location Parameter Test")) {
                  div(
                    class = "tests-control",
                    selectInput(
                      "tests_three_loc_choice",
                      "Select Tests",
                      choices = c("ANOVA" = "ANOVA", "Kruskal-Wallis" = "Kruskal-Wallis", "All" = "All"),
                      selected = three_loc_choice
                    )
                  )
                },
                div(
                  class = "tests-control",
                  numericInput("tests_three_alpha", "Significance level (α)", value = alpha_three, min = 0.001, max = 0.2, step = 0.001)
                )
              )
            )
          ),
          div(
            class = "tests-action-row",
            actionButton("tests_analyze", "Analyze Data", class = "btn tests-analyze-btn")
          )
        )
      ),
      if (analyzed && identical(sample_scope, "One Population Sample")) {
        active_num <- if (identical(test_type, "Normality Check")) normality_var else location_var
        x <- df[[active_num]]
        validate(need(is.numeric(x), "Choose a numeric variable."))
        x_clean <- x[is.finite(x)]
        validate(need(length(x_clean) >= 3, "Not enough numeric observations for inference (need at least 3)."))

        if (identical(test_type, "Normality Check")) {
          x_for_shapiro <- if (length(x_clean) > 5000) sample(x_clean, 5000) else x_clean
          sh <- shapiro.test(x_for_shapiro)
          is_normal <- sh$p.value > alpha_one
          summary_vals <- summary(x_clean)

          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", paste("Test Results for", active_num)),
              div(class = "badge", paste("α =", alpha_one))
            ),
            div(
              class = "test-card test-card-normality",
              div(
                class = "tests-card-head",
                h4("Normality Test (Shapiro-Wilk)"),
                span(class = "test-chip chip-diagnostic", "Diagnostic")
              ),
              div(
                class = "test-card-body",
                div(
                  class = "kv",
                  div(class = "k", "W statistic"), div(class = "v", round(sh$statistic, 4)),
                  div(class = "k", "p-value"), div(class = "v", fmt_p(sh$p.value)),
                  div(class = "k", "Sample size"), div(class = "v", length(x_for_shapiro)),
                  div(class = "k", "Decision threshold"), div(class = "v", paste0("alpha = ", alpha_one))
                ),
                div(
                  class = if (is_normal) "interpretation interpretation-success" else "interpretation interpretation-warning",
                  if (is_normal) paste0("p > alpha: fail to reject normality.") else paste0("p <= alpha: reject normality; data may be non-normal.")
                )
              )
            ),
            div(
              class = "test-card test-card-parametric",
              div(
                class = "tests-card-head",
                h4("Data Summary")
              ),
              div(
                class = "test-card-body",
                tags$table(
                  class = "tests-summary-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Statistic"),
                      tags$th("Value")
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td("Min."), tags$td(round(unname(summary_vals["Min."]), 4))),
                    tags$tr(tags$td("1st Qu."), tags$td(round(unname(summary_vals["1st Qu."]), 4))),
                    tags$tr(tags$td("Median"), tags$td(round(unname(summary_vals["Median"]), 4))),
                    tags$tr(tags$td("Mean"), tags$td(round(unname(summary_vals["Mean"]), 4))),
                    tags$tr(tags$td("3rd Qu."), tags$td(round(unname(summary_vals["3rd Qu."]), 4))),
                    tags$tr(tags$td("Max."), tags$td(round(unname(summary_vals["Max."]), 4)))
                  )
                )
              )
            )
          )
        } else {
          z_stat <- (mean(x_clean) - m0_value) / (sd(x_clean) / sqrt(length(x_clean)))
          z_crit <- qnorm(1 - alpha_one / 2)
          z_margin <- z_crit * (sd(x_clean) / sqrt(length(x_clean)))
          z_ci <- c(mean(x_clean) - z_margin, mean(x_clean) + z_margin)
          z_p <- switch(
            alt_value,
            "greater" = 1 - pnorm(z_stat),
            "less" = pnorm(z_stat),
            2 * (1 - pnorm(abs(z_stat)))
          )

          t_res <- t.test(x_clean, mu = m0_value, alternative = alt_value, conf.level = 1 - alpha_one)
          w_res <- wilcox.test(x_clean, mu = m0_value, alternative = alt_value, conf.int = TRUE, conf.level = 1 - alpha_one, exact = FALSE)

          show_z <- location_choice %in% c("z test", "All")
          show_t <- location_choice %in% c("t test", "All")
          show_w <- location_choice %in% c("Wilcoxon", "All")

          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", paste("Test Results for", active_num)),
              div(class = "badge", paste("α =", alpha_one))
            ),
            div(
              class = "tests-results-grid",
              if (show_z) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("One-Sample z test"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "z statistic"), div(class = "v", round(z_stat, 4)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(z_p)),
                      div(class = "k", "Hypothesized median (m₀)"), div(class = "v", m0_value),
                      div(class = "k", "Alternative"), div(class = "v", alt_value),
                      div(class = "k", "95% CI"), div(class = "v", paste0("[", round(z_ci[1], 4), ", ", round(z_ci[2], 4), "]"))
                    ),
                    div(
                      class = if (z_p <= alpha_one) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (z_p <= alpha_one) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              },
              if (show_t) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("One-Sample t test"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "t statistic"), div(class = "v", round(t_res$statistic, 4)),
                      div(class = "k", "df"), div(class = "v", round(t_res$parameter, 2)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(t_res$p.value)),
                      div(class = "k", "Sample mean"), div(class = "v", round(t_res$estimate, 4)),
                      div(class = "k", "Alternative"), div(class = "v", alt_value),
                      div(class = "k", "95% CI"), div(class = "v", paste0("[", round(t_res$conf.int[1], 4), ", ", round(t_res$conf.int[2], 4), "]"))
                    ),
                    div(
                      class = if (t_res$p.value <= alpha_one) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (t_res$p.value <= alpha_one) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              },
              if (show_w) {
                div(
                  class = "test-card test-card-nonparametric",
                  div(class = "tests-card-head", h4("Wilcoxon Signed-Rank"), span(class = "test-chip chip-nonparametric", "Non-parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "V statistic"), div(class = "v", round(w_res$statistic, 4)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(w_res$p.value)),
                      div(class = "k", "Alternative"), div(class = "v", alt_value),
                      div(class = "k", "Hypothesized median (m₀)"), div(class = "v", m0_value),
                      div(class = "k", "95% CI"),
                      div(
                        class = "v",
                        if (!is.null(w_res$conf.int) && length(w_res$conf.int) == 2) {
                          paste0("[", round(w_res$conf.int[1], 4), ", ", round(w_res$conf.int[2], 4), "]")
                        } else {
                          "Not available"
                        }
                      )
                    ),
                    div(
                      class = if (w_res$p.value <= alpha_one) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (w_res$p.value <= alpha_one) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              }
            )
          )
        }
      } else if (analyzed && identical(sample_scope, "Two Population Sample")) {
        x_data <- df[[two_x]]
        y_data <- df[[two_y]]
        validate(need(is.numeric(x_data) && is.numeric(y_data), "Select numeric columns for Sample X and Sample Y."))
        x_clean <- x_data[is.finite(x_data)]
        y_clean <- y_data[is.finite(y_data)]
        validate(need(length(x_clean) >= 3 && length(y_clean) >= 3, "Each sample needs at least 3 numeric observations."))

        x_for_shapiro <- if (length(x_clean) > 5000) sample(x_clean, 5000) else x_clean
        y_for_shapiro <- if (length(y_clean) > 5000) sample(y_clean, 5000) else y_clean

        if (identical(test_type_two, "Normality Check")) {
          sh_x <- shapiro.test(x_for_shapiro)
          sh_y <- shapiro.test(y_for_shapiro)
          reject_x <- sh_x$p.value <= alpha_two
          reject_y <- sh_y$p.value <= alpha_two

          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", "Normality Check Results"),
              div(class = "badge", paste("α =", alpha_two))
            ),
            div(
              class = "tests-results-grid",
              div(
                class = "test-card test-card-normality",
                div(class = "tests-card-head", h4(paste("Sample X:", two_x)), span(class = "test-chip chip-diagnostic", "Diagnostic")),
                div(
                  class = "test-card-body",
                  div(
                    class = "kv",
                    div(class = "k", "W statistic"), div(class = "v", round(sh_x$statistic, 4)),
                    div(class = "k", "p-value"), div(class = "v", fmt_p(sh_x$p.value)),
                    div(class = "k", "Sample size"), div(class = "v", length(x_for_shapiro)),
                    div(class = "k", "Decision threshold"), div(class = "v", paste0("alpha = ", alpha_two))
                  ),
                  div(
                    class = if (reject_x) "interpretation interpretation-warning" else "interpretation interpretation-success",
                    if (reject_x) paste0("p <= alpha: reject normality (Sample X).") else paste0("p > alpha: fail to reject normality (Sample X).")
                  )
                )
              ),
              div(
                class = "test-card test-card-normality",
                div(class = "tests-card-head", h4(paste("Sample Y:", two_y)), span(class = "test-chip chip-diagnostic", "Diagnostic")),
                div(
                  class = "test-card-body",
                  div(
                    class = "kv",
                    div(class = "k", "W statistic"), div(class = "v", round(sh_y$statistic, 4)),
                    div(class = "k", "p-value"), div(class = "v", fmt_p(sh_y$p.value)),
                    div(class = "k", "Sample size"), div(class = "v", length(y_for_shapiro)),
                    div(class = "k", "Decision threshold"), div(class = "v", paste0("alpha = ", alpha_two))
                  ),
                  div(
                    class = if (reject_y) "interpretation interpretation-warning" else "interpretation interpretation-success",
                    if (reject_y) paste0("p <= alpha: reject normality (Sample Y).") else paste0("p > alpha: fail to reject normality (Sample Y).")
                  )
                )
              )
            )
          )
        } else {
          diff_mean <- mean(x_clean) - mean(y_clean)
          nx <- length(x_clean)
          ny <- length(y_clean)
          se_diff <- sqrt((sd(x_clean)^2) / nx + (sd(y_clean)^2) / ny)

          z_stat <- (diff_mean - two_m0_value) / se_diff
          z_crit <- qnorm(1 - alpha_two / 2)
          z_margin <- z_crit * se_diff
          z_ci <- c(diff_mean - z_margin, diff_mean + z_margin)
          z_p <- switch(
            two_alt_value,
            "greater" = 1 - pnorm(z_stat),
            "less" = pnorm(z_stat),
            2 * (1 - pnorm(abs(z_stat)))
          )

          show_z <- two_loc_choice %in% c("z test", "All")
          show_t_equal <- two_loc_choice %in% c("t test equal", "All")
          show_t_welch <- two_loc_choice %in% c("t test welch", "All")
          show_w <- two_loc_choice %in% c("Wilcoxon", "All")

          t_equal_res <- if (show_t_equal) t.test(x_clean, y_clean, mu = two_m0_value, alternative = two_alt_value, conf.level = 1 - alpha_two, var.equal = TRUE) else NULL
          t_welch_res <- if (show_t_welch) t.test(x_clean, y_clean, mu = two_m0_value, alternative = two_alt_value, conf.level = 1 - alpha_two, var.equal = FALSE) else NULL
          w_res <- if (show_w) wilcox.test(x_clean, y_clean, mu = two_m0_value, alternative = two_alt_value, conf.int = TRUE, conf.level = 1 - alpha_two, exact = FALSE) else NULL

          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", paste("Location Parameter Test Results for", two_x, "and", two_y)),
              div(class = "badge", paste("α =", alpha_two))
            ),
            div(
              class = "tests-results-grid",
              if (show_z) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("Two-Sample z test"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "z statistic"), div(class = "v", round(z_stat, 4)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(z_p)),
                      div(class = "k", "Hypothesized difference (m₀)"), div(class = "v", two_m0_value),
                      div(class = "k", "Alternative"), div(class = "v", two_alt_value),
                      div(class = "k", "95% CI"), div(class = "v", paste0("[", round(z_ci[1], 4), ", ", round(z_ci[2], 4), "]")),
                      div(class = "k", "Sample sizes"), div(class = "v", paste0("nX=", nx, ", nY=", ny))
                    ),
                    div(
                      class = if (z_p <= alpha_two) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (z_p <= alpha_two) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              },
              if (show_t_equal) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("Two-Sample t test (equal variances)"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "t statistic"), div(class = "v", round(t_equal_res$statistic, 4)),
                      div(class = "k", "df"), div(class = "v", round(t_equal_res$parameter, 2)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(t_equal_res$p.value)),
                      div(class = "k", "Hypothesized difference (m₀)"), div(class = "v", two_m0_value),
                      div(class = "k", "Alternative"), div(class = "v", two_alt_value),
                      div(class = "k", "95% CI"), div(class = "v", paste0("[", round(t_equal_res$conf.int[1], 4), ", ", round(t_equal_res$conf.int[2], 4), "]"))
                    ),
                    div(
                      class = if (t_equal_res$p.value <= alpha_two) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (t_equal_res$p.value <= alpha_two) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              },
              if (show_t_welch) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("Two-Sample t test (Welch)"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "t statistic"), div(class = "v", round(t_welch_res$statistic, 4)),
                      div(class = "k", "df"), div(class = "v", round(t_welch_res$parameter, 2)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(t_welch_res$p.value)),
                      div(class = "k", "Hypothesized difference (m₀)"), div(class = "v", two_m0_value),
                      div(class = "k", "Alternative"), div(class = "v", two_alt_value),
                      div(class = "k", "95% CI"), div(class = "v", paste0("[", round(t_welch_res$conf.int[1], 4), ", ", round(t_welch_res$conf.int[2], 4), "]"))
                    ),
                    div(
                      class = if (t_welch_res$p.value <= alpha_two) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (t_welch_res$p.value <= alpha_two) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              },
              if (show_w) {
                div(
                  class = "test-card test-card-nonparametric",
                  div(class = "tests-card-head", h4("Wilcoxon (independent)"), span(class = "test-chip chip-nonparametric", "Non-parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "V statistic"), div(class = "v", round(w_res$statistic, 4)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(w_res$p.value)),
                      div(class = "k", "Hypothesized difference (m₀)"), div(class = "v", two_m0_value),
                      div(class = "k", "Alternative"), div(class = "v", two_alt_value),
                      div(class = "k", "95% CI"),
                      div(
                        class = "v",
                        if (!is.null(w_res$conf.int) && length(w_res$conf.int) == 2) {
                          paste0("[", round(w_res$conf.int[1], 4), ", ", round(w_res$conf.int[2], 4), "]")
                        } else {
                          "Not available"
                        }
                      )
                    ),
                    div(
                      class = if (w_res$p.value <= alpha_two) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (w_res$p.value <= alpha_two) "p <= alpha: reject H0." else "p > alpha: fail to reject H0."
                    )
                  )
                )
              }
            )
          )
        }
      } else if (analyzed && identical(sample_scope, "Three or more Population Sample")) {
        validate(need(length(three_vars) >= 3, "Select at least 3 numeric columns for 'Three or More'."))

        normality_rows <- lapply(three_vars, function(v) {
          vals <- df[[v]]
          validate(need(is.numeric(vals), paste0("Column '", v, "' must be numeric.")))
          vals_clean <- vals[is.finite(vals)]

          if (length(vals_clean) < 3) {
            return(list(variable = v, w = NA, p = NA, decision = "Not enough data"))
          }

          vals_for_shapiro <- if (length(vals_clean) > 5000) sample(vals_clean, 5000) else vals_clean
          sh <- shapiro.test(vals_for_shapiro)
          list(
            variable = v,
            w = round(unname(sh$statistic), 4),
            p = fmt_p(sh$p.value),
            decision = if (sh$p.value <= alpha_three) "Reject H0" else "Fail to reject H0"
          )
        })

        any_reject <- any(sapply(normality_rows, function(r) r$decision == "Reject H0"), na.rm = TRUE)

        if (identical(test_type_three, "Normality Check")) {
          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", "Normality Check Results"),
              div(class = "badge", paste("α =", alpha_three))
            ),
            div(
              class = "test-card test-card-parametric",
              div(class = "tests-card-head", h4("Grouped Summary")),
              div(
                class = "test-card-body",
                tags$table(
                  class = "tests-summary-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Group / Variable"),
                      tags$th("W statistic"),
                      tags$th("p-value"),
                      tags$th("Decision")
                    )
                  ),
                  tags$tbody(
                    lapply(normality_rows, function(r) {
                      tags$tr(
                        tags$td(r$variable),
                        tags$td(r$w),
                        tags$td(r$p),
                        tags$td(r$decision)
                      )
                    })
                  )
                )
              )
            ),
            div(
              class = if (any_reject) "interpretation interpretation-warning" else "interpretation interpretation-success",
              if (any_reject) "At least one selected variable shows evidence against normality at the chosen alpha." else "All selected variables fail to reject normality at the chosen alpha."
            )
          )
        } else {
          groups_vals <- lapply(three_vars, function(v) df[[v]][is.finite(df[[v]])])
          validate(need(all(sapply(groups_vals, length) >= 3), "Each selected group needs at least 3 numeric observations for location tests."))

          y_all <- unlist(groups_vals, use.names = FALSE)
          g_all <- factor(rep(three_vars, times = sapply(groups_vals, length)))

          show_anova <- three_loc_choice %in% c("ANOVA", "All")
          show_kruskal <- three_loc_choice %in% c("Kruskal-Wallis", "All")

          anova_res <- if (show_anova) aov(y_all ~ g_all) else NULL
          anova_tbl <- if (show_anova) summary(anova_res)[[1]] else NULL

          kruskal_res <- if (show_kruskal) kruskal.test(y_all ~ g_all) else NULL

          div(
            class = "tests-panel tests-results-panel",
            div(
              class = "tests-results-header",
              div(class = "tests-section-title", "Location Parameter Test Results"),
              div(class = "badge", paste("α =", alpha_three))
            ),
            div(
              class = "tests-results-grid",
              if (show_anova) {
                div(
                  class = "test-card test-card-parametric",
                  div(class = "tests-card-head", h4("One-Way ANOVA"), span(class = "test-chip chip-parametric", "Parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "F statistic"), div(class = "v", round(unname(anova_tbl$`F value`[1]), 4)),
                      div(class = "k", "df1"), div(class = "v", round(anova_tbl$Df[1], 2)),
                      div(class = "k", "df2"), div(class = "v", round(anova_tbl$Df[2], 2)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(anova_tbl$`Pr(>F)`[1]))
                    ),
                    div(
                      class = if (anova_tbl$`Pr(>F)`[1] <= alpha_three) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (anova_tbl$`Pr(>F)`[1] <= alpha_three) "p <= alpha: reject H0 (means differ)." else "p > alpha: fail to reject H0 (means equal)."
                    )
                  )
                )
              },
              if (show_kruskal) {
                div(
                  class = "test-card test-card-nonparametric",
                  div(class = "tests-card-head", h4("Kruskal-Wallis"), span(class = "test-chip chip-nonparametric", "Non-parametric")),
                  div(
                    class = "test-card-body",
                    div(
                      class = "kv",
                      div(class = "k", "Chi-squared statistic"), div(class = "v", round(unname(kruskal_res$statistic), 4)),
                      div(class = "k", "df"), div(class = "v", round(unname(kruskal_res$parameter), 2)),
                      div(class = "k", "p-value"), div(class = "v", fmt_p(kruskal_res$p.value))
                    ),
                    div(
                      class = if (kruskal_res$p.value <= alpha_three) "interpretation interpretation-success" else "interpretation interpretation-info",
                      if (kruskal_res$p.value <= alpha_three) "p <= alpha: reject H0 (distributions differ)." else "p > alpha: fail to reject H0 (distributions equal)."
                    )
                  )
                )
              }
            )
          )
        }
      }
    )
  })
}
