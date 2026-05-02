register_regression_server <- function(input, output, session, state) {
  current_regression_mode <- reactiveVal("Single Regression")

  build_regression_mode_tab <- function(id, title, active = FALSE, disabled = FALSE) {
    tab_class <- paste(
      "btn inferential-tab-button",
      if (active) "is-active" else "",
      if (disabled) "is-disabled" else ""
    )

    actionButton(
      id,
      label = span(class = "action-label inferential-tab-title", title),
      class = tab_class,
      style = "width: auto;",
      disabled = disabled
    )
  }

  normalize_regression_frame <- function(df, y_var, x_vars) {
    vars_needed <- c(y_var, x_vars)
    df_model <- df[, vars_needed, drop = FALSE]
    df_model <- df_model[complete.cases(df_model), , drop = FALSE]
    list(df = df_model, vars = vars_needed)
  }

  build_explicit_model <- function(df, y_var, x_vars, mode) {
    if (!(length(y_var) == 1 && !is.na(y_var) && nzchar(y_var))) {
      stop("Choose a response variable.")
    }
    if (length(x_vars) < 1) {
      stop("Choose at least one predictor.")
    }
    if (y_var %in% x_vars) {
      stop("Response variable cannot also be a predictor.")
    }

    x_vars <- unique(x_vars)
    model_frame <- normalize_regression_frame(df, y_var, x_vars)
    df_model <- model_frame$df

    if (nrow(df_model) < 3) {
      stop("Need at least 3 complete observations to build the model.")
    }

    formula_str <- paste0("`", y_var, "` ~ ", paste0("`", x_vars, "`", collapse = " + "))
    formula_obj <- as.formula(formula_str, env = parent.frame())
    model <- lm(formula = formula_obj, data = df_model, y = TRUE, qr = TRUE, model = TRUE)
    model$call$formula <- formula_obj
    model$call$data <- quote(df_model)

    list(
      mode = mode,
      model = model,
      df = df_model,
      y_var = y_var,
      x_vars = x_vars,
      formula_display = paste(y_var, "~", paste(x_vars, collapse = " + ")),
      built_at = Sys.time()
    )
  }

  safe_normality_test <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)

    if (n < 3) {
      return(list(
        label = "Normality Check",
        method = "Unavailable",
        statistic_name = "Statistic",
        statistic = NA_real_,
        p.value = NA_real_,
        note = "Need at least 3 residuals for a normality test."
      ))
    }

    if (length(unique(round(x, 10))) < 3) {
      return(list(
        label = "Normality Check",
        method = "Unavailable",
        statistic_name = "Statistic",
        statistic = NA_real_,
        p.value = NA_real_,
        note = "Residuals do not have enough variation for a stable normality test."
      ))
    }

    if (n <= 5000) {
      test <- shapiro.test(x)
      return(list(
        label = "Normality Check",
        method = "Shapiro-Wilk",
        statistic_name = "W statistic",
        statistic = unname(test$statistic),
        p.value = test$p.value,
        note = NULL
      ))
    }

    test <- nortest::ad.test(x)
    list(
      label = "Normality Check",
      method = "Anderson-Darling",
      statistic_name = "A statistic",
      statistic = unname(test$statistic),
      p.value = test$p.value,
      note = "Used Anderson-Darling because Shapiro-Wilk only supports sample sizes up to 5000."
    )
  }

  regression_metrics <- function(model) {
    y_obs <- model$model[[1]]
    rss <- sum(residuals(model)^2, na.rm = TRUE)
    mse <- mean(residuals(model)^2, na.rm = TRUE)
    press_val <- sum((residuals(model) / (1 - hatvalues(model)))^2, na.rm = TRUE)
    tss <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
    pred_r2 <- if (tss > 0) 1 - press_val / tss else NA_real_

    list(
      rss = rss,
      mse = mse,
      press = press_val,
      pred_r2 = pred_r2,
      r2 = summary(model)$r.squared,
      adj_r2 = summary(model)$adj.r.squared,
      rse = summary(model)$sigma,
      fstat = summary(model)$fstatistic[1],
      aic = AIC(model),
      bic = BIC(model)
    )
  }

  built_regression <- reactive({
    state$built_regression()
  })

  current_built_model <- reactive({
    model_info <- built_regression()
    req(model_info)
    model_info
  })

  active_mode_model <- reactive({
    model_info <- built_regression()
    req(model_info)
    req(identical(model_info$mode, current_regression_mode()))
    model_info
  })

  adequacy_model <- reactive({
    current_built_model()$model
  })

  adequacy_predictor_df <- reactive({
    model <- adequacy_model()
    mf <- model.frame(model)

    if (ncol(mf) < 3) {
      return(NULL)
    }

    x_df <- mf[, -1, drop = FALSE]
    x_df <- x_df[, sapply(x_df, is.numeric), drop = FALSE]

    if (ncol(x_df) < 2) {
      return(NULL)
    }

    x_df
  })

  adequacy_pairwise_corr <- reactive({
    x_df <- adequacy_predictor_df()
    req(!is.null(x_df))

    cm <- cor(x_df, use = "complete.obs")
    cm[upper.tri(cm, diag = TRUE)] <- NA

    idx <- which(!is.na(cm), arr.ind = TRUE)
    if (nrow(idx) == 0) {
      return(NULL)
    }

    out <- data.frame(
      Predictor_1 = rownames(cm)[idx[, 1]],
      Predictor_2 = colnames(cm)[idx[, 2]],
      Correlation = cm[idx],
      Abs_Correlation = abs(cm[idx]),
      stringsAsFactors = FALSE
    )

    out <- out[order(out$Abs_Correlation, decreasing = TRUE), ]
    rownames(out) <- NULL
    out
  })

  penalized_data <- reactive({
    built_model <- built_regression()

    validate(need(!is.null(built_model), "Build a regression model first on the Regression page."))
    validate(need(identical(built_model$mode, "Multiple Regression"), "Ridge, lasso, and cross-validation are available for the built multiple regression model only."))
    validate(need(length(built_model$x_vars) >= 2, "Penalized regression requires at least 2 predictors."))
    validate(need(requireNamespace("glmnet", quietly = TRUE), "Package 'glmnet' is required for ridge, lasso, and cross-validation."))

    dat <- built_model$df[, c(built_model$y_var, built_model$x_vars), drop = FALSE]
    dat <- dat[complete.cases(dat), , drop = FALSE]

    validate(need(nrow(dat) >= 10, "Need at least 10 complete observations for penalized regression."))

    list(
      x = as.matrix(dat[, built_model$x_vars, drop = FALSE]),
      y = dat[[built_model$y_var]],
      y_var = built_model$y_var,
      x_vars = built_model$x_vars,
      dat = dat
    )
  })

  ridge_cv <- reactive({
    pd <- penalized_data()
    glmnet::cv.glmnet(pd$x, pd$y, alpha = 0, standardize = TRUE)
  })

  lasso_cv <- reactive({
    pd <- penalized_data()
    glmnet::cv.glmnet(pd$x, pd$y, alpha = 1, standardize = TRUE)
  })

  observeEvent(input$regression_mode_single, {
    current_regression_mode("Single Regression")
  }, ignoreInit = TRUE)

  observeEvent(input$regression_mode_multiple, {
    if (length(state$numeric_vars()) >= 3) {
      current_regression_mode("Multiple Regression")
    } else {
      showNotification("Multiple regression needs at least 3 numeric variables.", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)

  observe({
    req(state$data_in())

    if (length(state$numeric_vars()) < 3 && identical(current_regression_mode(), "Multiple Regression")) {
      current_regression_mode("Single Regression")
    }
  })

  observeEvent(input$build_slr_model, {
    req(state$data_in())
    tryCatch({
      model_info <- build_explicit_model(
        df = state$data_in(),
        y_var = input$slr_y,
        x_vars = input$slr_x,
        mode = "Single Regression"
      )

      state$built_regression(model_info)
      current_regression_mode("Single Regression")
      showNotification("Single regression model built successfully.", type = "message", duration = 2)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 4)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$build_mlr_model, {
    req(state$data_in())
    if (length(input$mlr_x) < 2) {
      showNotification("Multiple regression needs at least 2 predictors.", type = "warning", duration = 4)
      return()
    }

    tryCatch({
      model_info <- build_explicit_model(
        df = state$data_in(),
        y_var = input$mlr_y,
        x_vars = input$mlr_x,
        mode = "Multiple Regression"
      )

      state$built_regression(model_info)
      current_regression_mode("Multiple Regression")
      showNotification("Multiple regression model built successfully.", type = "message", duration = 2)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 4)
    })
  }, ignoreInit = TRUE)

  output$regression_ui <- renderUI({
    req(state$data_in())
    nums <- state$numeric_vars()
    validate(need(length(nums) >= 2, "Need at least 2 numeric variables for regression analysis."))

    built_model <- built_regression()
    selected_mode <- current_regression_mode()
    multiple_available <- length(nums) >= 3

    slr_y_choices <- nums
    slr_y_selected <- if (!is.null(input$slr_y) && input$slr_y %in% slr_y_choices) input$slr_y else slr_y_choices[1]
    slr_x_choices <- setdiff(nums, slr_y_selected)
    slr_x_selected <- if (!is.null(input$slr_x) && input$slr_x %in% slr_x_choices) input$slr_x else slr_x_choices[1]

    mlr_y_choices <- nums
    mlr_y_selected <- if (!is.null(input$mlr_y) && input$mlr_y %in% mlr_y_choices) input$mlr_y else mlr_y_choices[1]
    mlr_x_choices <- setdiff(nums, mlr_y_selected)
    mlr_x_selected <- if (!is.null(input$mlr_x)) {
      intersect(input$mlr_x, mlr_x_choices)
    } else {
      head(mlr_x_choices, 2)
    }

    tagList(
      div(
        class = "tests-page-shell regression-page-shell",
        div(
          class = "tests-page-hero",
          h2("Regression"),
          p("Select exactly the variables you want to model, build the regression, and then reuse that same model in adequacy checks and correction workflows.")
        ),
        div(
          class = paste(
            "inferential-tab-shell regression-tab-shell",
            if (identical(selected_mode, "Single Regression")) "regression-scope-single" else ""
          ),
          div(
            class = "inferential-tab-row regression-tab-row",
            build_regression_mode_tab(
              "regression_mode_single",
              "Single Regression",
              active = identical(selected_mode, "Single Regression")
            ),
            build_regression_mode_tab(
              "regression_mode_multiple",
              "Multiple Regression",
              active = identical(selected_mode, "Multiple Regression"),
              disabled = !multiple_available
            )
          )
        ),
        div(
          class = "inferential-results-shell regression-results-shell",
          div(
            class = "tests-panel tests-input-panel regression-builder-panel",
            div(class = "tests-section-title", "Model Selection"),
            if (identical(selected_mode, "Single Regression")) {
              tagList(
                div(
                  class = "tests-input-grid regression-builder-grid regression-builder-grid-single",
                  div(class = "tests-control", selectInput("slr_y", "Response (Y)", choices = slr_y_choices, selected = slr_y_selected)),
                  div(class = "tests-control", selectInput("slr_x", "Predictor (X)", choices = slr_x_choices, selected = slr_x_selected)),
                  div(
                    class = "tests-control regression-build-action",
                    actionButton("build_slr_model", "Build Model", class = "btn tests-analyze-btn regression-build-btn")
                  )
                )
              )
            } else {
              tagList(
                if (!multiple_available) {
                  div(
                    class = "info-card",
                    HTML("Multiple regression becomes available once your dataset has at least <strong>3 numeric variables</strong>.")
                  )
                },
                div(
                  class = "tests-input-grid regression-builder-grid regression-builder-grid-multiple",
                  div(class = "tests-control", selectInput("mlr_y", "Response (Y)", choices = mlr_y_choices, selected = mlr_y_selected)),
                  div(class = "tests-control regression-predictor-control", selectizeInput("mlr_x", "Predictors (X)", choices = mlr_x_choices, selected = mlr_x_selected, multiple = TRUE)),
                  div(
                    class = "tests-control regression-build-action",
                    actionButton("build_mlr_model", "Build Model", class = "btn tests-analyze-btn regression-build-btn", disabled = !multiple_available)
                  )
                )
              )
            },
            if (is.null(built_model)) {
              div(
                class = "info-card regression-builder-note",
                HTML("Build a regression model first. That exact model will then be reused automatically in <strong>Model Adequacy</strong> and <strong>Correct Inadequacies</strong>.")
              )
            } else if (!identical(built_model$mode, selected_mode)) {
              div(
                class = "info-card regression-builder-note",
                HTML(paste0(
                  "You currently have a built <strong>", built_model$mode, "</strong> model: <strong>",
                  built_model$formula_display,
                  "</strong>. Build a ", tolower(selected_mode), " model here to replace it."
                ))
              )
            }
          ),
          if (!is.null(built_model) && identical(built_model$mode, selected_mode)) {
            if (identical(selected_mode, "Single Regression")) uiOutput("slr_ui") else uiOutput("mlr_ui")
          }
        )
      )
    )
  })

  slr_data <- reactive({
    model_info <- active_mode_model()
    req(identical(model_info$mode, "Single Regression"))

    list(
      model = model_info$model,
      df = model_info$df,
      x = model_info$df[[model_info$x_vars[1]]],
      y = model_info$df[[model_info$y_var]],
      x_var = model_info$x_vars[1],
      y_var = model_info$y_var,
      formula_display = model_info$formula_display
    )
  })

  mlr_data <- reactive({
    model_info <- active_mode_model()
    req(identical(model_info$mode, "Multiple Regression"))
    model_info
  })

  output$slr_ui <- renderUI({
    m_data <- slr_data()
    model <- m_data$model
    m_summ <- summary(model)
    eval_stats <- regression_metrics(model)
    cor_p <- cor.test(m_data$x, m_data$y, method = "pearson")
    cor_s <- cor.test(m_data$x, m_data$y, method = "spearman")
    cor_k <- cor.test(m_data$x, m_data$y, method = "kendall")

    tagList(
      div(
        class = "tests-panel regression-lead-panel",
        div(
          class = "card-header regression-card-header",
          div(class = "regression-card-title", paste0("📈 Simple Linear Regression: ", m_data$formula_display))
        ),
        div(
          class = "info-card",
          HTML(paste0(
            "<strong>Model:</strong> ", m_data$y_var, " = β₀ + β₁ × ", m_data$x_var, "<br>",
            "<strong>Equation:</strong> ŷ = ", round(coef(model)[1], 3), " + ", round(coef(model)[2], 3), "x"
          ))
        ),
        div(
          class = "visual-stats regression-metrics-grid",
          div(
            class = "visual-stat-item",
            h4("📊 Model Coefficients"),
            div(
              class = "kv",
              div(class = "k", "Intercept (β₀)"), div(class = "v", round(coef(model)[1], 4)),
              div(class = "k", "Slope (β₁)"), div(class = "v", round(coef(model)[2], 4)),
              div(class = "k", "β₀ p-value"), div(class = "v", fmt_p(m_summ$coefficients[1, 4])),
              div(class = "k", "β₁ p-value"), div(class = "v", fmt_p(m_summ$coefficients[2, 4]))
            )
          ),
          div(
            class = "visual-stat-item",
            h4("📈 Model Performance"),
            div(
              class = "kv",
              div(class = "k", "R²"), div(class = "v", round(m_summ$r.squared, 4)),
              div(class = "k", "Adj. R²"), div(class = "v", round(m_summ$adj.r.squared, 4)),
              div(class = "k", "RSE"), div(class = "v", round(m_summ$sigma, 4)),
              div(class = "k", "F-statistic"), div(class = "v", round(m_summ$fstatistic[1], 2))
            )
          ),
          div(
            class = "visual-stat-item",
            h4("🎯 Model Evaluation"),
            div(
              class = "kv",
              div(class = "k", "MSE"), div(class = "v", round(eval_stats$mse, 4)),
              div(class = "k", "PRESS"), div(class = "v", round(eval_stats$press, 2)),
              div(class = "k", "Pred R²"), div(class = "v", round(eval_stats$pred_r2, 4)),
              div(class = "k", "AIC"), div(class = "v", round(eval_stats$aic, 2)),
              div(class = "k", "BIC"), div(class = "v", round(eval_stats$bic, 2))
            )
          ),
          div(
            class = "visual-stat-item",
            h4("🔗 Correlation Analysis"),
            div(
              class = "kv",
              div(class = "k", "Pearson r"), div(class = "v", round(cor_p$estimate, 4)),
              div(class = "k", "Pearson p"), div(class = "v", fmt_p(cor_p$p.value)),
              div(class = "k", "Spearman ρ"), div(class = "v", round(cor_s$estimate, 4)),
              div(class = "k", "Kendall τ"), div(class = "v", round(cor_k$estimate, 4))
            )
          ),
          div(
            class = "visual-stat-item full-width-card",
            h4("📝 Overall Conclusion"),
            div(
              class = "info-card",
              HTML(paste0(
                "<strong>Fit quality:</strong> ",
                if (eval_stats$r2 >= 0.7) "Strong" else if (eval_stats$r2 >= 0.4) "Moderate" else "Weak",
                "<br><strong>Best use:</strong> ",
                if (m_summ$coefficients[2, 4] < 0.05) "Useful for inference and prediction" else "Limited predictive or inferential value",
                "<br><strong>Model note:</strong> Compare AIC, BIC, and MSE when choosing between candidate models."
              ))
            )
          )
        )
      ),
      div(class = "content-card", div(class = "card-header regression-card-header", div(class = "regression-card-title", "📊 ANOVA Table")), div(class = "table-card", DTOutput("slr_anova"))),
      div(class = "content-card", div(class = "card-header regression-card-header", div(class = "regression-card-title", "📋 Coefficients Summary")), div(class = "table-card", DTOutput("slr_coef"))),
      div(
        class = "content-card",
        div(class = "card-header regression-card-header", div(class = "regression-card-title", "📈 Regression Plot")),
        withSpinner(plotOutput("slr_plot", height = "500px"), color = "#6366f1", type = 4),
        div(class = "download-section", downloadButton("download_slr_plot", "📥 Download Plot", class = "btn btn-info"))
      )
    )
  })

  output$slr_anova <- renderDT({
    anova_tbl <- anova(slr_data()$model)
    DT::datatable(anova_tbl, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = TRUE)
  })

  output$slr_coef <- renderDT({
    coef_tbl <- summary(slr_data()$model)$coefficients
    coef_tbl <- cbind(coef_tbl, confint(slr_data()$model))
    colnames(coef_tbl) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "CI 2.5%", "CI 97.5%")
    DT::datatable(round(coef_tbl, 4), options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = TRUE)
  })

  output$slr_plot <- renderPlot({
    m <- slr_data()
    plot(m$x, m$y, xlab = m$x_var, ylab = m$y_var, main = paste("Simple Linear Regression:", m$formula_display), pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.6), cex = 1.2, cex.main = 1.4, cex.lab = 1.2, col.main = "#111827")
    abline(m$model, col = "#ef4444", lwd = 3)
    grid(col = "gray85", lty = 1)
    legend("topleft", legend = paste0("R² = ", round(summary(m$model)$r.squared, 4)), bty = "n", cex = 1.2, text.col = "#111827")
  })

  output$download_slr_plot <- downloadHandler(
    filename = function() paste0("slr_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 900, res = 120)
      m <- slr_data()
      plot(m$x, m$y, xlab = m$x_var, ylab = m$y_var, main = paste("Simple Linear Regression:", m$formula_display), pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.6), cex = 1.2)
      abline(m$model, col = "#ef4444", lwd = 3)
      legend("topleft", legend = paste0("R² = ", round(summary(m$model)$r.squared, 4)), bty = "n")
      dev.off()
    }
  )

  output$mlr_ui <- renderUI({
    m_data <- mlr_data()
    model <- m_data$model
    df <- m_data$df
    y_var <- m_data$y_var
    x_vars <- m_data$x_vars
    m_summ <- summary(model)
    eval_stats <- regression_metrics(model)

    vif_vals <- NULL
    if (length(x_vars) > 1) {
      vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
    }

    tagList(
      div(
        class = "tests-panel regression-lead-panel",
        div(class = "card-header regression-card-header", div(class = "regression-card-title", "📉 Multiple Linear Regression")),
        div(class = "info-card", HTML(paste0("<strong>Response Variable:</strong> ", y_var, "<br><strong>Predictors:</strong> ", paste(x_vars, collapse = ", "), "<br><strong>Model:</strong> ", m_data$formula_display))),
        div(
          class = "visual-stats regression-metrics-grid",
          div(class = "visual-stat-item", h4("📈 Model Performance"), div(class = "kv", div(class = "k", "R²"), div(class = "v", round(m_summ$r.squared, 4)), div(class = "k", "Adj. R²"), div(class = "v", round(m_summ$adj.r.squared, 4)), div(class = "k", "RSE"), div(class = "v", round(m_summ$sigma, 4)), div(class = "k", "F-statistic"), div(class = "v", round(m_summ$fstatistic[1], 2)))),
          div(class = "visual-stat-item", h4("🎯 Model Evaluation"), div(class = "kv", div(class = "k", "MSE"), div(class = "v", round(eval_stats$mse, 4)), div(class = "k", "PRESS"), div(class = "v", round(eval_stats$press, 2)), div(class = "k", "Pred R²"), div(class = "v", round(eval_stats$pred_r2, 4)), div(class = "k", "AIC"), div(class = "v", round(eval_stats$aic, 2)), div(class = "k", "BIC"), div(class = "v", round(eval_stats$bic, 2)))),
          div(class = "visual-stat-item", h4("📊 Model Statistics"), div(class = "kv", div(class = "k", "Observations"), div(class = "v", length(model$residuals)), div(class = "k", "Predictors"), div(class = "v", length(x_vars)), div(class = "k", "DF Residual"), div(class = "v", model$df.residual), div(class = "k", "p-value"), div(class = "v", fmt_p(pf(m_summ$fstatistic[1], m_summ$fstatistic[2], m_summ$fstatistic[3], lower.tail = FALSE)))))
        )
      ),
      div(class = "content-card", div(class = "card-header regression-card-header", div(class = "regression-card-title", "📋 Coefficients Summary")), div(class = "table-card", DTOutput("mlr_coef"))),
      div(class = "content-card", div(class = "card-header regression-card-header", div(class = "regression-card-title", "📊 ANOVA Table")), div(class = "table-card", DTOutput("mlr_anova"))),
      div(class = "content-card", div(class = "card-header regression-card-header", div(class = "regression-card-title", "📊 Scatter Plot Matrix")), withSpinner(plotOutput("mlr_pairs", height = "600px"), color = "#6366f1", type = 4))
    )
  })

  output$mlr_coef <- renderDT({
    coef_tbl <- summary(mlr_data()$model)$coefficients
    coef_tbl <- cbind(coef_tbl, confint(mlr_data()$model))
    colnames(coef_tbl) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "CI 2.5%", "CI 97.5%")
    DT::datatable(round(coef_tbl, 4), options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = TRUE)
  })

  output$mlr_anova <- renderDT({
    anova_tbl <- anova(mlr_data()$model)
    DT::datatable(anova_tbl, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = TRUE)
  })

  output$mlr_vif <- renderPlot({
    m <- mlr_data()
    tryCatch({
      vif_vals <- car::vif(m$model)
      barplot(vif_vals, main = "VIF Values - Multicollinearity Check", ylab = "VIF", col = "#6366f1", border = "white", ylim = c(0, max(11, max(vif_vals) * 1.1)), las = 2, cex.names = 0.9, cex.main = 1.4, col.main = "#111827")
      abline(h = 5, lwd = 2, lty = 2, col = "#f59e0b")
      abline(h = 10, lwd = 2, lty = 2, col = "#ef4444")
      legend("topright", legend = c("VIF = 5 (Concern)", "VIF = 10 (Severe)"), col = c("#f59e0b", "#ef4444"), lty = 2, lwd = 2, bty = "n")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "VIF cannot be calculated\n(possible perfect collinearity)", cex = 1.5, col = "#ef4444")
    })
  })

  output$mlr_pairs <- renderPlot({
    m <- mlr_data()
    pairs(m$df, main = "Scatter Plot Matrix", pch = 19, col = adjustcolor("#6366f1", alpha.f = 0.5), cex = 0.8, col.main = "#111827", cex.main = 1.4)
  })

  candidate_model_metrics <- reactive({
    m <- mlr_data()
    x_vars <- m$x_vars
    df <- m$df
    y_var <- m$y_var

    if (length(x_vars) < 2) {
      return(NULL)
    }

    x_vars_eval <- x_vars[seq_len(min(length(x_vars), 10))]
    full_formula <- as.formula(paste0("`", y_var, "` ~ ", paste0("`", x_vars_eval, "`", collapse = " + ")))
    full_model <- lm(full_formula, data = df)
    sigma2_full <- summary(full_model)$sigma^2
    n <- nrow(model.frame(full_model))

    combos <- unlist(
      lapply(seq_along(x_vars_eval), function(k) combn(x_vars_eval, k, simplify = FALSE)),
      recursive = FALSE
    )

    out <- lapply(combos, function(vars) {
      form <- as.formula(paste0("`", y_var, "` ~ ", paste0("`", vars, "`", collapse = " + ")))
      fit <- lm(form, data = df)
      rss <- sum(residuals(fit)^2, na.rm = TRUE)
      p <- length(coef(fit))
      mse <- mean(residuals(fit)^2, na.rm = TRUE)

      data.frame(
        Model = paste(vars, collapse = " + "),
        Predictors = length(vars),
        R2 = summary(fit)$r.squared,
        Adj_R2 = summary(fit)$adj.r.squared,
        MSE = mse,
        Mallows_Cp = rss / sigma2_full - (n - 2 * p),
        AIC = AIC(fit),
        BIC = BIC(fit),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, out)
    out <- out[order(out$BIC, out$AIC, out$Mallows_Cp), ]
    rownames(out) <- NULL
    out
  })

  output$model_candidates <- renderDT({
    tbl <- candidate_model_metrics()
    validate(need(!is.null(tbl), "Candidate model comparison requires at least 2 predictors."))

    tbl_display <- tbl
    numeric_cols <- sapply(tbl_display, is.numeric)
    tbl_display[numeric_cols] <- lapply(tbl_display[numeric_cols], round, 4)

    DT::datatable(tbl_display, options = list(pageLength = 8, scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$model_eval_summary <- renderUI({
    tbl <- candidate_model_metrics()

    if (is.null(tbl) || nrow(tbl) == 0) {
      return(div(class = "info-card", HTML("Candidate-model evaluation is available when multiple predictors are present.")))
    }

    best <- tbl[1, ]
    div(class = "info-card success", HTML(paste0("<strong>Best candidate model summary</strong><br><strong>Model:</strong> ", best$Model, "<br><strong>Adj. R²:</strong> ", round(best$Adj_R2, 4), "<br><strong>MSE:</strong> ", round(best$MSE, 4), "<br><strong>Mallows' Cp:</strong> ", round(best$Mallows_Cp, 4), "<br><strong>AIC:</strong> ", round(best$AIC, 4), "<br><strong>BIC:</strong> ", round(best$BIC, 4))))
  })

  output$adequacy_ui <- renderUI({
    built_model <- built_regression()

    if (is.null(built_model)) {
      return(
        div(
          class = "content-card",
          div(class = "card-header", "✅ Model Adequacy"),
          div(class = "info-card", HTML("Build a regression model first on the <strong>Regression</strong> page. Model adequacy checks always use the exact model you built there."))
        )
      )
    }

    model <- built_model$model
    normality_test <- safe_normality_test(residuals(model))
    bp_test <- lmtest::bptest(model)
    dw_test <- lmtest::dwtest(model)
    reset_test <- tryCatch(lmtest::resettest(model, power = 2:3, type = "regressor"), error = function(e) NULL)

    tagList(
      div(class = "content-card", div(class = "card-header", paste0("✅ Model Adequacy Check - ", built_model$mode)), div(class = "info-card", HTML(paste0("<strong>Built Model:</strong> ", built_model$formula_display, "<br><strong>Assumptions Tested:</strong> Normality, Homoscedasticity, Independence, Linearity")))),
      div(
        class = "content-card",
        div(class = "card-header", "🔬 Assumption Tests"),
        div(
          class = "test-grid",
          div(class = "test-card", h4("1️⃣ Normality of Residuals"), div(class = "test-card-body", div(class = "kv", div(class = "k", "Test"), div(class = "v", normality_test$method), div(class = "k", normality_test$statistic_name), div(class = "v", if (is.na(normality_test$statistic)) "NA" else round(normality_test$statistic, 4)), div(class = "k", "p-value"), div(class = "v", fmt_p(normality_test$p.value))), div(class = "interpretation", if (!is.null(normality_test$note)) normality_test$note else if (normality_test$p.value > 0.05) "✅ Residuals appear normal (p > 0.05)" else "⚠️ Residuals may not be normal (p < 0.05)"))),
          div(class = "test-card", h4("2️⃣ Equal Variance"), div(class = "test-card-body", div(class = "kv", div(class = "k", "Test"), div(class = "v", "Breusch-Pagan"), div(class = "k", "BP statistic"), div(class = "v", round(bp_test$statistic, 4)), div(class = "k", "p-value"), div(class = "v", fmt_p(bp_test$p.value))), div(class = "interpretation", if (bp_test$p.value > 0.05) "✅ Constant variance (p > 0.05)" else "⚠️ Heteroscedasticity detected (p < 0.05)"))),
          div(class = "test-card", h4("3️⃣ Independence"), div(class = "test-card-body", div(class = "kv", div(class = "k", "Test"), div(class = "v", "Durbin-Watson"), div(class = "k", "DW statistic"), div(class = "v", round(dw_test$statistic, 4)), div(class = "k", "p-value"), div(class = "v", fmt_p(dw_test$p.value))), div(class = "interpretation", "DW ≈ 2 indicates no autocorrelation. Values far from 2 suggest dependence."))),
          if (!is.null(reset_test)) div(class = "test-card", h4("4️⃣ Linearity"), div(class = "test-card-body", div(class = "kv", div(class = "k", "Test"), div(class = "v", "RESET"), div(class = "k", "F statistic"), div(class = "v", round(reset_test$statistic, 4)), div(class = "k", "p-value"), div(class = "v", fmt_p(reset_test$p.value))), div(class = "interpretation", if (reset_test$p.value > 0.05) "✅ Linear relationship (p > 0.05)" else "⚠️ May need non-linear terms (p < 0.05)")))
        )
      ),
      div(class = "content-card", div(class = "card-header", "📊 Diagnostic Plots"), fluidRow(column(6, withSpinner(plotOutput("adequacy_resid1", height = "400px"), color = "#6366f1", type = 4)), column(6, withSpinner(plotOutput("adequacy_resid2", height = "400px"), color = "#6366f1", type = 4))), fluidRow(column(6, withSpinner(plotOutput("adequacy_resid3", height = "400px"), color = "#6366f1", type = 4)), column(6, withSpinner(plotOutput("adequacy_resid4", height = "400px"), color = "#6366f1", type = 4)))),
      div(class = "content-card", div(class = "card-header", "📝 Model Adequacy Summary"), uiOutput("adequacy_summary"))
    )
  })

  output$adequacy_corr_table <- renderDT({
    corr_tbl <- adequacy_pairwise_corr()
    validate(need(!is.null(corr_tbl), "Pair-wise correlation requires at least 2 numeric predictors."))

    corr_display <- corr_tbl[, c("Predictor_1", "Predictor_2", "Correlation", "Abs_Correlation")]
    numeric_cols <- sapply(corr_display, is.numeric)
    corr_display[numeric_cols] <- lapply(corr_display[numeric_cols], round, 4)

    DT::datatable(corr_display, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$adequacy_corr_plot <- renderPlot({
    x_df <- adequacy_predictor_df()
    validate(need(!is.null(x_df), "Need at least 2 numeric predictors for a correlation plot."))

    corrplot::corrplot(cor(x_df, use = "complete.obs"), method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.8)
  })

  output$adequacy_multicoll_summary <- renderUI({
    corr_tbl <- adequacy_pairwise_corr()

    if (is.null(corr_tbl) || nrow(corr_tbl) == 0) {
      return(div(class = "info-card", HTML("<strong>Pair-wise correlation not available</strong><br>This diagnostic requires at least 2 numeric predictors.")))
    }

    max_abs <- max(corr_tbl$Abs_Correlation, na.rm = TRUE)
    flagged <- subset(corr_tbl, Abs_Correlation >= 0.8)

    if (max_abs < 0.7) {
      div(class = "info-card success", HTML(paste0("<strong>✅ Low multicollinearity concern</strong><br>Largest absolute pair-wise correlation = <strong>", round(max_abs, 4), "</strong>.")))
    } else if (max_abs < 0.8) {
      div(class = "info-card", HTML(paste0("<strong>ℹ️ Moderate multicollinearity concern</strong><br>Largest absolute pair-wise correlation = <strong>", round(max_abs, 4), "</strong>. Monitor correlated predictors.")))
    } else {
      top_pairs <- apply(head(flagged, 3), 1, function(r) {
        paste0(r["Predictor_1"], " & ", r["Predictor_2"], " (", round(as.numeric(r["Correlation"]), 4), ")")
      })
      div(class = "info-card warning", HTML(paste0("<strong>⚠️ Potential multicollinearity detected</strong><br>Largest absolute pair-wise correlation = <strong>", round(max_abs, 4), "</strong><br><strong>Most correlated pairs:</strong><br>", paste(top_pairs, collapse = "<br>"))))
    }
  })

  output$adequacy_resid1 <- renderPlot({
    plot(adequacy_model(), which = 1, col = adjustcolor("#6366f1", alpha.f = 0.6), pch = 19, cex = 1.2, col.main = "#111827", cex.main = 1.3)
  })

  output$adequacy_resid2 <- renderPlot({
    plot(adequacy_model(), which = 2, col = adjustcolor("#6366f1", alpha.f = 0.6), pch = 19, cex = 1.2, col.main = "#111827", cex.main = 1.3)
  })

  output$adequacy_resid3 <- renderPlot({
    plot(adequacy_model(), which = 3, col = adjustcolor("#6366f1", alpha.f = 0.6), pch = 19, cex = 1.2, col.main = "#111827", cex.main = 1.3)
  })

  output$adequacy_resid4 <- renderPlot({
    plot(adequacy_model(), which = 5, col = adjustcolor("#6366f1", alpha.f = 0.6), pch = 19, cex = 1.2, col.main = "#111827", cex.main = 1.3)
  })

  output$adequacy_summary <- renderUI({
    model <- adequacy_model()
    normality_test <- safe_normality_test(residuals(model))
    bp_test <- lmtest::bptest(model)
    dw_test <- lmtest::dwtest(model)

    normality_ok <- !is.na(normality_test$p.value) && normality_test$p.value > 0.05
    homoscedasticity_ok <- bp_test$p.value > 0.05
    independence_ok <- abs(dw_test$statistic - 2) < 0.5
    all_ok <- normality_ok && homoscedasticity_ok && independence_ok

    if (all_ok) {
      div(class = "info-card success", HTML("<strong>✅ Model Assumptions Satisfied</strong><br>All diagnostic tests passed. The built model is adequate for inference and prediction."))
    } else {
      issues <- character(0)
      if (!normality_ok) issues <- c(issues, "Normality of residuals")
      if (!homoscedasticity_ok) issues <- c(issues, "Constant variance")
      if (!independence_ok) issues <- c(issues, "Independence of errors")

      div(class = "info-card warning", HTML(paste0("<strong>⚠️ Model Assumptions Violated</strong><br>Issues detected: ", paste(issues, collapse = ", "), "<br>Consider: transformations, robust regression, or different model specification.")))
    }
  })

  output$boxcox_plot <- renderPlot({
    model <- adequacy_model()
    y_vals <- model.response(model.frame(model))

    if (min(y_vals, na.rm = TRUE) <= 0) {
      plot.new()
      text(0.5, 0.5, "Box-Cox requires positive Y values\nShift your data if needed", cex = 1.5, col = "#ef4444")
    } else {
      boxcox(model, plotit = TRUE, lambda = seq(-2, 2, by = 0.1))
      title(main = "Box-Cox Transformation", col.main = "#111827", cex.main = 1.4)
      grid(col = "gray85", lty = 1)
    }
  })

  output$boxcox_info <- renderUI({
    built_model <- built_regression()

    if (is.null(built_model)) {
      return(div(class = "info-card", HTML("Build a regression model first on the <strong>Regression</strong> page to use correction methods.")))
    }

    model <- adequacy_model()
    y_vals <- model.response(model.frame(model))

    if (min(y_vals, na.rm = TRUE) <= 0) {
      div(class = "info-card warning", HTML("⚠ Cannot compute Box-Cox: the built model response contains zero or negative values."))
    } else {
      bc <- boxcox(model, plotit = FALSE, lambda = seq(-2, 2, by = 0.1))
      lambda_opt <- bc$x[which.max(bc$y)]

      transform_text <- if (abs(lambda_opt - 1) < 0.1) {
        "No transformation needed (λ ≈ 1)"
      } else if (abs(lambda_opt) < 0.1) {
        "Log transformation suggested (λ ≈ 0)"
      } else if (abs(lambda_opt - 0.5) < 0.1) {
        "Square root transformation suggested (λ ≈ 0.5)"
      } else if (abs(lambda_opt + 1) < 0.1) {
        "Inverse transformation suggested (λ ≈ -1)"
      } else {
        paste0("Power transformation with λ = ", round(lambda_opt, 3))
      }

      div(class = "info-card success", HTML(paste0("✓ Optimal λ = ", round(lambda_opt, 3), "<br><strong>", transform_text, "</strong>")))
    }
  })

  output$box_tidwell <- renderUI({
    built_model <- built_regression()

    if (is.null(built_model)) {
      return(div(class = "info-card", HTML("Build a regression model first on the <strong>Regression</strong> page to inspect correction methods.")))
    }

    model <- adequacy_model()

    tryCatch({
      if (length(coef(model)) < 2) {
        return(div(class = "info-card warning", HTML("⚠️ Box-Tidwell test requires at least one predictor variable.")))
      }

      model_data <- model.frame(model)
      response_name <- names(model_data)[1]
      predictor_names <- names(model_data)[-1]
      form <- as.formula(paste0("`", response_name, "` ~ ", paste0("`", predictor_names, "`", collapse = " + ")))
      bt_result <- capture.output(car::boxTidwell(form, data = model_data))

      predictor_results <- lapply(predictor_names, function(pred) {
        pred_lines <- grep(pred, bt_result, value = TRUE)
        if (length(pred_lines) > 0) {
          values <- strsplit(pred_lines[1], "\\s+")[[1]]
          values <- values[values != ""]
          list(
            predictor = pred,
            lambda = if (length(values) >= 2) values[2] else "N/A",
            pvalue = if (length(values) >= 4) values[4] else "N/A"
          )
        }
      })

      tagList(
        div(
          class = "visual-stats",
          lapply(predictor_results, function(result) {
            if (!is.null(result)) {
              p_val <- suppressWarnings(as.numeric(result$pvalue))
              is_linear <- !is.na(p_val) && p_val > 0.05
              div(class = "visual-stat-item", h4(result$predictor), div(class = "kv", div(class = "k", "MLE of λ"), div(class = "v", result$lambda), div(class = "k", "p-value"), div(class = "v", result$pvalue), div(class = "k", "Interpretation"), div(class = "v", if (is_linear) "✅ Linear" else "⚠️ May need transformation")))
            }
          })
        ),
        div(class = "info-card", style = "margin-top: 20px;", HTML("<strong>Full Output:</strong>"), tags$pre(style = "background: #f3f4f6; padding: 12px; border-radius: 6px; font-size: 12px; overflow-x: auto;", paste(bt_result, collapse = "\n")))
      )
    }, error = function(e) {
      div(class = "info-card warning", HTML(paste0("<strong>⚠️ Box-Tidwell test could not be performed</strong><br><strong>Possible reasons:</strong><br>• Predictor variables must be strictly positive<br>• Model may not be suitable for this test<br><br><strong>Error:</strong> ", e$message)))
    })
  })

  output$wls_summary <- renderUI({
    built_model <- built_regression()

    if (is.null(built_model)) {
      return(div(class = "info-card", HTML("Build a regression model first on the <strong>Regression</strong> page to try weighted least squares.")))
    }

    m_ols <- adequacy_model()
    weights_inv <- 1 / (fitted(m_ols)^2)

    if (any(is.infinite(weights_inv)) || any(is.na(weights_inv))) {
      return(div(class = "info-card warning", HTML("<strong>⚠️ Cannot fit WLS model</strong><br>Fitted values are too close to zero. Try another model or a transformation first.")))
    }

    m_wls <- tryCatch(lm(formula(m_ols), data = model.frame(m_ols), weights = weights_inv), error = function(e) NULL)
    if (is.null(m_wls)) {
      return(div(class = "info-card warning", HTML("<strong>⚠️ WLS fitting failed</strong><br>The weighted model could not be fitted for the built regression.")))
    }

    bp_ols <- lmtest::bptest(m_ols)
    bp_wls <- lmtest::bptest(m_wls)

    div(
      div(class = "visual-stats", div(class = "visual-stat-item", h4("OLS vs WLS Comparison"), div(class = "kv", div(class = "k", "OLS R²"), div(class = "v", round(summary(m_ols)$r.squared, 4)), div(class = "k", "WLS R²"), div(class = "v", round(summary(m_wls)$r.squared, 4)), div(class = "k", "OLS RSE"), div(class = "v", round(summary(m_ols)$sigma, 4)), div(class = "k", "WLS RSE"), div(class = "v", round(summary(m_wls)$sigma, 4)))), div(class = "visual-stat-item", h4("Heteroscedasticity Tests"), div(class = "kv", div(class = "k", "OLS BP p-value"), div(class = "v", fmt_p(bp_ols$p.value)), div(class = "k", "WLS BP p-value"), div(class = "v", fmt_p(bp_wls$p.value)), div(class = "k", "Improvement"), div(class = "v", if (bp_wls$p.value > bp_ols$p.value) "✅ Better" else "⚠️ Worse")))),
      div(class = "info-card", style = "margin-top: 20px;", HTML(paste0("<strong>Interpretation:</strong><br>", if (bp_wls$p.value > 0.05 && bp_ols$p.value < 0.05) "✅ WLS successfully corrected heteroscedasticity" else if (bp_wls$p.value > bp_ols$p.value) "✓ WLS shows improvement in variance homogeneity" else "⚠️ WLS did not improve heteroscedasticity. Consider other transformations.")))
    )
  })

  output$ridge_cv_plot <- renderPlot({
    plot(ridge_cv(), main = "Ridge Cross-Validation")
  })

  output$lasso_cv_plot <- renderPlot({
    plot(lasso_cv(), main = "Lasso Cross-Validation")
  })

  output$ridge_summary <- renderUI({
    cv_fit <- ridge_cv()
    ridge_mse <- min(cv_fit$cvm, na.rm = TRUE)

    div(class = "visual-stats", div(class = "visual-stat-item", h4("Ridge CV Results"), div(class = "kv", div(class = "k", "Best λ (lambda.min)"), div(class = "v", round(cv_fit$lambda.min, 6)), div(class = "k", "1-SE λ"), div(class = "v", round(cv_fit$lambda.1se, 6)), div(class = "k", "Min CV MSE"), div(class = "v", round(ridge_mse, 6)), div(class = "k", "Interpretation"), div(class = "v", "Ridge shrinks coefficients but keeps all predictors in the model."))))
  })

  output$lasso_summary <- renderUI({
    cv_fit <- lasso_cv()
    coef_min <- coef(cv_fit, s = "lambda.min")
    non_zero <- sum(as.vector(coef_min) != 0) - 1
    lasso_mse <- min(cv_fit$cvm, na.rm = TRUE)

    div(class = "visual-stats", div(class = "visual-stat-item", h4("Lasso CV Results"), div(class = "kv", div(class = "k", "Best λ (lambda.min)"), div(class = "v", round(cv_fit$lambda.min, 6)), div(class = "k", "1-SE λ"), div(class = "v", round(cv_fit$lambda.1se, 6)), div(class = "k", "Min CV MSE"), div(class = "v", round(lasso_mse, 6)), div(class = "k", "Selected Predictors"), div(class = "v", non_zero))))
  })

  output$ridge_coef <- renderDT({
    coef_mat <- as.matrix(coef(ridge_cv(), s = "lambda.min"))
    coef_df <- data.frame(Term = rownames(coef_mat), Estimate = round(as.numeric(coef_mat[, 1]), 6), stringsAsFactors = FALSE)
    DT::datatable(coef_df, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$lasso_coef <- renderDT({
    coef_mat <- as.matrix(coef(lasso_cv(), s = "lambda.min"))
    coef_df <- data.frame(Term = rownames(coef_mat), Estimate = round(as.numeric(coef_mat[, 1]), 6), stringsAsFactors = FALSE)
    DT::datatable(coef_df, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$cv_compare <- renderUI({
    pd <- penalized_data()
    ols_formula <- as.formula(paste0("`", pd$y_var, "` ~ ", paste0("`", pd$x_vars, "`", collapse = " + ")))
    ols_model <- lm(ols_formula, data = pd$dat)
    ols_mse <- mean(residuals(ols_model)^2, na.rm = TRUE)
    ridge_mse <- min(ridge_cv()$cvm, na.rm = TRUE)
    lasso_mse <- min(lasso_cv()$cvm, na.rm = TRUE)

    method_names <- c("OLS", "Ridge", "Lasso")
    mse_vals <- c(ols_mse, ridge_mse, lasso_mse)
    best_method <- method_names[which.min(mse_vals)]

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Model Comparison"),
        div(
          class = "kv",
          div(class = "k", "OLS MSE"), div(class = "v", round(ols_mse, 6)),
          div(class = "k", "Ridge CV MSE"), div(class = "v", round(ridge_mse, 6)),
          div(class = "k", "Lasso CV MSE"), div(class = "v", round(lasso_mse, 6)),
          div(class = "k", "Best Method"), div(class = "v", best_method)
        )
      ),
      div(
        class = "visual-stat-item",
        h4("Conclusion"),
        HTML(paste0("<strong>", best_method, "</strong> has the smallest error in this comparison. Use it as the preferred model for prediction."))
      )
    )
  })

  output$cv_compare_plot <- renderPlot({
    pd <- penalized_data()
    ols_formula <- as.formula(paste0("`", pd$y_var, "` ~ ", paste0("`", pd$x_vars, "`", collapse = " + ")))
    ols_model <- lm(ols_formula, data = pd$dat)
    ols_rmse <- sqrt(mean(residuals(ols_model)^2, na.rm = TRUE))
    ridge_rmse <- sqrt(min(ridge_cv()$cvm, na.rm = TRUE))
    lasso_rmse <- sqrt(min(lasso_cv()$cvm, na.rm = TRUE))

    rmses <- c(ols_rmse, ridge_rmse, lasso_rmse)
    barplot(
      rmses,
      names.arg = c("OLS", "Ridge", "LASSO"),
      col = "#6366f1", border = "white",
      ylab = "10-fold CV RMSE", main = "Model Comparison",
      las = 1, cex.names = 1.0, cex.main = 1.4, col.main = "#111827",
      ylim = c(0, max(rmses) * 1.15)
    )
  })

  multicoll_vif <- reactive({
    built_model <- built_regression()
    validate(need(!is.null(built_model), "Build a regression model first on the Regression page."))
    validate(need(identical(built_model$mode, "Multiple Regression"), "VIF requires the built multiple regression model."))
    validate(need(length(built_model$x_vars) >= 2, "VIF requires at least 2 predictors."))
    tryCatch(car::vif(built_model$model), error = function(e) NULL)
  })

  output$multicoll_vif_summary <- renderUI({
    vif_vals <- multicoll_vif()
    if (is.null(vif_vals)) {
      return(div(class = "info-card warning", HTML("<strong>Status:</strong> VIF could not be calculated (possible perfect collinearity).")))
    }
    max_vif <- max(vif_vals)
    high_count <- sum(vif_vals > 5)
    severity_class <- if (max_vif < 5) "info-card success" else if (max_vif < 10) "info-card" else "info-card warning"
    interpretation <- if (max_vif < 5) {
      "Low multicollinearity. Predictors are not strongly correlated."
    } else if (max_vif < 10) {
      "Moderate multicollinearity. Monitor correlated predictors and consider Ridge regression."
    } else {
      "High multicollinearity. Consider Ridge, Lasso, or removing/redefining correlated predictors."
    }

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("VIF Overview"),
        div(
          class = "kv",
          div(class = "k", "Predictors"), div(class = "v", length(vif_vals)),
          div(class = "k", "Max VIF"), div(class = "v", round(max_vif, 4)),
          div(class = "k", "Mean VIF"), div(class = "v", round(mean(vif_vals), 4)),
          div(class = "k", "High VIF Count (>5)"), div(class = "v", high_count)
        ),
        div(class = severity_class, style = "margin-top: 14px;", HTML(paste0("<strong>Interpretation:</strong> ", interpretation)))
      )
    )
  })

  output$multicoll_vif_plot <- renderPlot({
    vif_vals <- multicoll_vif()
    validate(need(!is.null(vif_vals), "VIF cannot be calculated (possible perfect collinearity)."))
    y_max <- max(11, max(vif_vals) * 1.15)
    barplot(
      vif_vals,
      main = "VIF Values - Multicollinearity Check",
      ylab = "VIF",
      col = "#6366f1", border = "white",
      ylim = c(0, y_max),
      las = 2, cex.names = 0.9, cex.main = 1.4, col.main = "#111827"
    )
    abline(h = 5, lwd = 2, lty = 3, col = "#f59e0b")
    abline(h = 10, lwd = 2, lty = 3, col = "#ef4444")
    legend("topright", legend = c("VIF = 5 (Concern)", "VIF = 10 (Severe)"), col = c("#f59e0b", "#ef4444"), lty = 3, lwd = 2, bty = "n")
  })

  mb_full_model <- reactive({
    built_model <- built_regression()
    validate(need(!is.null(built_model), "Build a regression model first on the Regression page."))
    validate(need(identical(built_model$mode, "Multiple Regression"), "Model Building requires a multiple regression model with at least 2 predictors."))
    validate(need(length(built_model$x_vars) >= 2, "Model Building requires at least 2 predictors."))
    built_model
  })

  output$mb_full_stats <- renderUI({
    bm <- mb_full_model()
    s <- summary(bm$model)
    f <- s$fstatistic
    p_val <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA
    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item",
        h4("Model Fit"),
        div(
          class = "kv",
          div(class = "k", "Observations"), div(class = "v", length(bm$model$residuals)),
          div(class = "k", "Predictors"), div(class = "v", length(bm$x_vars)),
          div(class = "k", "R²"), div(class = "v", round(s$r.squared, 4)),
          div(class = "k", "Adjusted R²"), div(class = "v", round(s$adj.r.squared, 4)),
          div(class = "k", "Residual Std. Error"), div(class = "v", round(s$sigma, 4)),
          div(class = "k", "F statistic"), div(class = "v", if (!is.null(f)) round(f[1], 3) else "N/A"),
          div(class = "k", "Model p-value"), div(class = "v", if (!is.na(p_val)) fmt_p(p_val) else "N/A")
        )
      )
    )
  })

  output$mb_full_coef <- renderDT({
    bm <- mb_full_model()
    cm <- summary(bm$model)$coefficients
    df <- data.frame(
      Term = rownames(cm),
      Estimate = round(cm[, 1], 5),
      `Std. Error` = round(cm[, 2], 5),
      `t value` = round(cm[, 3], 4),
      `Pr(>|t|)` = vapply(cm[, 4], fmt_p, character(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$mb_full_anova_tbl <- renderDT({
    a <- as.data.frame(anova(mb_full_model()$model))
    a$Term <- rownames(a)
    a <- a[, c("Term", setdiff(names(a), "Term")), drop = FALSE]
    num_cols <- vapply(a, is.numeric, logical(1))
    a[num_cols] <- lapply(a[num_cols], function(x) ifelse(is.na(x), NA, round(x, 4)))
    if ("Pr(>F)" %in% names(a)) a[["Pr(>F)"]] <- vapply(a[["Pr(>F)"]], function(p) if (is.na(p)) "" else fmt_p(p), character(1))
    DT::datatable(a, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$mb_corr_plot <- renderPlot({
    bm <- mb_full_model()
    dat <- bm$df[, c(bm$y_var, bm$x_vars), drop = FALSE]
    dat <- dat[complete.cases(dat), , drop = FALSE]
    validate(need(nrow(dat) >= 3, "Need at least 3 complete observations."))
    cm <- cor(dat)
    if (requireNamespace("corrplot", quietly = TRUE)) {
      corrplot::corrplot.mixed(
        cm, lower = "number", upper = "circle", outline = TRUE,
        mar = c(1, 1, 1, 1), tl.cex = 0.9, tl.col = "black",
        cl.cex = 0.7, cl.ratio = 0.2, number.cex = 0.9, number.digits = 3
      )
    } else {
      heatmap(cm, symm = TRUE, main = "Correlation Matrix")
    }
  })

  mb_all_subsets <- reactive({
    bm <- mb_full_model()
    validate(need(requireNamespace("olsrr", quietly = TRUE), "Package 'olsrr' is required for best subset selection."))
    res <- tryCatch(olsrr::ols_step_all_possible(bm$model), error = function(e) NULL)
    validate(need(!is.null(res), "Best subset selection failed for this model."))
    df <- if (is.data.frame(res)) res else res$result
    df
  })

  output$mb_all_subsets <- renderDT({
    df <- mb_all_subsets()
    keep <- intersect(c("n", "predictors", "rsquare", "adjr", "predrsq", "cp", "aic", "sbc", "msep"), names(df))
    out <- df[, keep, drop = FALSE]
    num_cols <- vapply(out, is.numeric, logical(1))
    out[num_cols] <- lapply(out[num_cols], function(x) round(x, 4))
    DT::datatable(
      out,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE),
      class = "cell-border stripe hover",
      rownames = FALSE
    )
  })

  output$mb_best_summary <- renderUI({
    df <- mb_all_subsets()
    pick <- function(col, fn) {
      if (!col %in% names(df)) return("N/A")
      idx <- fn(df[[col]])
      if (length(idx) == 0 || is.na(idx)) return("N/A")
      as.character(df$predictors[idx])
    }
    best_mse <- pick("msep", function(x) which.min(x))
    best_adjr <- pick("adjr", function(x) which.max(x))
    best_predrsq <- pick("predrsq", function(x) which.max(x))
    best_aic <- pick("aic", function(x) which.min(x))
    best_bic <- pick("sbc", function(x) which.min(x))
    best_cp <- "N/A"
    if (all(c("cp", "n") %in% names(df))) {
      cpp <- df$cp - (df$n + 1)
      idx <- which.min(abs(cpp))
      if (length(idx) > 0) best_cp <- as.character(df$predictors[idx])
    }

    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item full-width-card",
        h4("Best Model by Criterion"),
        div(
          class = "kv",
          div(class = "k", "Min MSEp"), div(class = "v", best_mse),
          div(class = "k", "Max Adjusted R²"), div(class = "v", best_adjr),
          div(class = "k", "Max Predicted R²"), div(class = "v", best_predrsq),
          div(class = "k", "Cp closest to p"), div(class = "v", best_cp),
          div(class = "k", "Min AIC"), div(class = "v", best_aic),
          div(class = "k", "Min BIC"), div(class = "v", best_bic)
        )
      )
    )
  })

  output$mb_cp_plot <- renderPlot({
    df <- mb_all_subsets()
    validate(need(all(c("cp", "n") %in% names(df)), "Cp values not available."))
    p_vals <- df$n + 1
    plot(
      p_vals, df$cp,
      xlab = "p = k + 1", ylab = "Mallows' Cp",
      main = "Cp vs p (line: Cp = p)",
      pch = 19, col = "#6366f1",
      ylim = c(0, max(df$cp, p_vals, na.rm = TRUE) * 1.1)
    )
    abline(a = 0, b = 1, col = "#ef4444", lwd = 2)
  })

  mb_stepwise_ui <- function(fn, ...) {
    bm <- mb_full_model()
    validate(need(requireNamespace("olsrr", quietly = TRUE), "Package 'olsrr' is required for stepwise selection."))
    res <- tryCatch(fn(bm$model, ...), error = function(e) e)
    if (inherits(res, "error")) {
      return(div(class = "info-card warning", HTML(paste0("<strong>Procedure failed:</strong> ", conditionMessage(res)))))
    }

    extract <- function(obj) {
      candidates <- c("predictors", "indvar", "model$variable")
      for (slot in c("predictors", "indvar")) {
        if (!is.null(obj[[slot]])) return(as.character(obj[[slot]]))
      }
      if (!is.null(obj$model)) {
        return(setdiff(all.vars(formula(obj$model))[-1], character(0)))
      }
      character(0)
    }
    selected <- extract(res)

    step_df <- NULL
    if (!is.null(res$steps) || !is.null(res$step)) {
      candidates <- list(res$steps, res$step, res$metrics)
      for (cand in candidates) {
        if (is.data.frame(cand) && nrow(cand) > 0) { step_df <- cand; break }
      }
    }
    if (is.null(step_df) && !is.null(res$metrics) && is.data.frame(res$metrics)) step_df <- res$metrics

    final_formula <- if (length(selected) > 0) paste0("`", bm$y_var, "` ~ ", paste0("`", selected, "`", collapse = " + ")) else NA

    div(
      div(
        class = "visual-stats",
        div(
          class = "visual-stat-item full-width-card",
          h4("Selection Result"),
          div(
            class = "kv",
            div(class = "k", "Selected predictors"), div(class = "v", if (length(selected) > 0) paste(selected, collapse = ", ") else "(none)"),
            div(class = "k", "Number selected"), div(class = "v", length(selected)),
            div(class = "k", "Final formula"), div(class = "v", if (!is.na(final_formula)) final_formula else "N/A")
          )
        )
      ),
      if (!is.null(step_df)) {
        num_cols <- vapply(step_df, is.numeric, logical(1))
        step_df[num_cols] <- lapply(step_df[num_cols], function(x) round(x, 4))
        div(
          class = "table-card",
          h4("Step-by-Step Trace"),
          DT::datatable(step_df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
        )
      }
    )
  }

  output$mb_forward_ui <- renderUI({
    mb_stepwise_ui(olsrr::ols_step_forward_p, penter = 0.10, details = FALSE)
  })

  output$mb_backward_ui <- renderUI({
    mb_stepwise_ui(olsrr::ols_step_backward_p, prem = 0.15, details = FALSE)
  })

  output$mb_stepwise_ui <- renderUI({
    mb_stepwise_ui(olsrr::ols_step_both_p, pent = 0.10, prem = 0.15, details = FALSE)
  })

  mb_final_predictors <- reactive({
    df <- mb_all_subsets()
    validate(need(all(c("cp", "n", "predictors") %in% names(df)), "Cannot derive final model."))
    cpp <- df$cp - (df$n + 1)
    idx <- which.min(abs(cpp))
    preds <- as.character(df$predictors[idx])
    strsplit(preds, "\\s+")[[1]]
  })

  mb_final_model <- reactive({
    bm <- mb_full_model()
    preds <- mb_final_predictors()
    validate(need(length(preds) >= 1, "No predictors selected."))
    formula_str <- paste0("`", bm$y_var, "` ~ ", paste0("`", preds, "`", collapse = " + "))
    lm(as.formula(formula_str), data = bm$df)
  })

  output$mb_final_stats <- renderUI({
    m <- mb_final_model()
    s <- summary(m)
    f <- s$fstatistic
    p_val <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA
    preds <- mb_final_predictors()
    div(
      class = "visual-stats",
      div(
        class = "visual-stat-item full-width-card",
        h4("Final Model"),
        div(
          class = "kv",
          div(class = "k", "Selected predictors"), div(class = "v", paste(preds, collapse = ", ")),
          div(class = "k", "Observations"), div(class = "v", length(m$residuals)),
          div(class = "k", "R²"), div(class = "v", round(s$r.squared, 4)),
          div(class = "k", "Adjusted R²"), div(class = "v", round(s$adj.r.squared, 4)),
          div(class = "k", "Residual Std. Error"), div(class = "v", round(s$sigma, 4)),
          div(class = "k", "F statistic"), div(class = "v", if (!is.null(f)) round(f[1], 3) else "N/A"),
          div(class = "k", "Model p-value"), div(class = "v", if (!is.na(p_val)) fmt_p(p_val) else "N/A")
        )
      )
    )
  })

  output$mb_final_coef <- renderDT({
    m <- mb_final_model()
    cm <- summary(m)$coefficients
    df <- data.frame(
      Term = rownames(cm),
      Estimate = round(cm[, 1], 5),
      `Std. Error` = round(cm[, 2], 5),
      `t value` = round(cm[, 3], 4),
      `Pr(>|t|)` = vapply(cm[, 4], fmt_p, character(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$mb_final_vif_tbl <- renderDT({
    m <- mb_final_model()
    if (length(coef(m)) - 1 < 2) {
      df <- data.frame(Note = "VIF requires at least 2 predictors. Final model has fewer.", stringsAsFactors = FALSE)
    } else {
      v <- car::vif(m)
      df <- data.frame(
        Predictor = names(v),
        VIF = round(as.numeric(v), 4),
        Status = ifelse(v < 5, "Low", ifelse(v < 10, "Moderate", "High")),
        stringsAsFactors = FALSE
      )
    }
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE), class = "cell-border stripe hover", rownames = FALSE)
  })
}
