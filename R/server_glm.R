register_glm_server <- function(input, output, session, state) {

  # ── UI ──────────────────────────────────────────────────────────────────────
  output$glm_ui <- renderUI({
    req(state$data_in())
    df   <- state$data_in()
    all_vars  <- names(df)
    nums <- state$numeric_vars()

    tagList(
      div(
        class = "tests-page-shell",
        div(
          class = "tests-page-hero",
          h2("Generalized Linear Models (GLM)"),
          p("Fit logistic regression (logit and probit links) for binary outcomes,
             and Poisson regression for count data. Includes coefficient tables,
             likelihood-ratio tests, model fit statistics, and prediction grids.")
        )
      ),

      # ── Model type selector ────────────────────────────────────────────────
      div(
        class = "content-card",
        div(class = "card-header", "⚙️ Model Configuration"),
        div(class = "tests-input-grid regression-builder-grid",
            div(class = "tests-control",
                selectInput("glm_type", "GLM Family",
                            choices = c("Logistic (Logit)" = "logit",
                                        "Logistic (Probit)" = "probit",
                                        "Both Logit & Probit" = "both",
                                        "Poisson (Count)" = "poisson"),
                            selected = "both")),
            div(class = "tests-control",
                selectInput("glm_y", "Response Variable (Y)", choices = all_vars, selected = all_vars[1])),
            div(class = "tests-control",
                selectizeInput("glm_x", "Predictor(s) (X)",
                               choices  = setdiff(all_vars, all_vars[1]),
                               selected = if (length(all_vars) >= 2) all_vars[2] else NULL,
                               multiple = TRUE)),
            div(class = "tests-control",
                selectizeInput("glm_factors", "Factor Variable(s) (optional)",
                               choices  = all_vars,
                               selected = NULL,
                               multiple = TRUE,
                               options  = list(placeholder = "Select categorical predictors"))),
            div(class = "tests-control regression-build-action",
                actionButton("glm_run", "Fit GLM", class = "btn tests-analyze-btn regression-build-btn"))
        ),
        uiOutput("glm_y_check")
      ),

      uiOutput("glm_results_ui")
    )
  })

  # ── Y variable check ─────────────────────────────────────────────────────────
  output$glm_y_check <- renderUI({
    req(state$data_in(), input$glm_y, input$glm_type)
    df <- state$data_in()
    y  <- df[[input$glm_y]]
    y  <- y[!is.na(y)]

    if (input$glm_type %in% c("logit", "probit", "both")) {
      uvals <- sort(unique(y))
      if (length(uvals) == 2 && all(uvals %in% c(0, 1))) {
        div(class = "info-card success",
            HTML(paste0("✅ <strong>", input$glm_y, "</strong> is binary (0/1) — suitable for logistic regression.")))
      } else {
        div(class = "info-card warning",
            HTML(paste0("⚠️ <strong>", input$glm_y, "</strong> has values: [",
                        paste(head(uvals, 6), collapse = ", "), if (length(uvals) > 6) "…" else "", "].
                        Logistic regression requires a binary (0/1) response.")))
      }
    } else {
      uvals <- sort(unique(y))
      if (all(y == floor(y)) && min(y) >= 0) {
        div(class = "info-card success",
            HTML(paste0("✅ <strong>", input$glm_y, "</strong> contains non-negative integers — suitable for Poisson regression.")))
      } else {
        div(class = "info-card warning",
            HTML(paste0("⚠️ Poisson regression requires non-negative integer counts.")))
      }
    }
  })

  # ── Update predictor choices when Y changes ───────────────────────────────────
  observeEvent(input$glm_y, {
    req(state$data_in())
    choices <- setdiff(names(state$data_in()), input$glm_y)
    updateSelectizeInput(session, "glm_x", choices = choices, selected = head(choices, 1))
    updateSelectizeInput(session, "glm_factors", choices = choices, selected = NULL)
  })

  # ── Reactive: prepare data & fit models ──────────────────────────────────────
  glm_models <- eventReactive(input$glm_run, {
    req(state$data_in(), input$glm_y, input$glm_x)
    validate(need(!input$glm_y %in% input$glm_x, "Response cannot also be a predictor."))

    df    <- state$data_in()
    y_var <- input$glm_y
    x_vars <- input$glm_x
    fac_vars <- input$glm_factors

    # Convert factors
    for (fv in fac_vars) {
      if (fv %in% names(df)) df[[fv]] <- factor(df[[fv]])
    }

    # Keep only needed columns
    use_vars <- unique(c(y_var, x_vars))
    df_use   <- df[, use_vars, drop = FALSE]
    df_use   <- df_use[complete.cases(df_use), , drop = FALSE]
    validate(need(nrow(df_use) >= 10, "Need at least 10 complete observations."))

    # Build formula
    formula_str <- paste0("`", y_var, "` ~ ",
                          paste0("`", x_vars, "`", collapse = " + "))
    fmla <- as.formula(formula_str)

    glm_type <- input$glm_type

    results <- list(
      df       = df_use,
      y_var    = y_var,
      x_vars   = x_vars,
      fac_vars = fac_vars,
      type     = glm_type,
      formula_display = paste(y_var, "~", paste(x_vars, collapse = " + "))
    )

    if (glm_type %in% c("logit", "both")) {
      results$m_logit <- tryCatch(
        glm(fmla, family = binomial(link = "logit"), data = df_use),
        error = function(e) { validate(need(FALSE, paste("Logit model error:", e$message))) }
      )
    }
    if (glm_type %in% c("probit", "both")) {
      results$m_probit <- tryCatch(
        glm(fmla, family = binomial(link = "probit"), data = df_use),
        error = function(e) { validate(need(FALSE, paste("Probit model error:", e$message))) }
      )
    }
    if (glm_type == "poisson") {
      results$m_poisson <- tryCatch(
        glm(fmla, family = "poisson", data = df_use),
        error = function(e) { validate(need(FALSE, paste("Poisson model error:", e$message))) }
      )
    }

    results
  })

  # ── Helper: LR test ──────────────────────────────────────────────────────────
  lr_test_ui <- function(mod) {
    chi  <- mod$null.deviance - mod$deviance
    dof  <- mod$df.null - mod$df.residual
    pval <- pchisq(chi, dof, lower.tail = FALSE)
    div(class = "kv",
        div(class = "k", "Null deviance"),   div(class = "v", round(mod$null.deviance, 3)),
        div(class = "k", "Residual deviance"),div(class = "v", round(mod$deviance, 3)),
        div(class = "k", "LR χ²"),           div(class = "v", round(chi, 3)),
        div(class = "k", "df"),              div(class = "v", dof),
        div(class = "k", "LR p-value"),      div(class = "v", fmt_p(pval)),
        div(class = "k", "AIC"),             div(class = "v", round(AIC(mod), 3)),
        div(class = "k", "McFadden R²"),     div(class = "v", round(1 - mod$deviance / mod$null.deviance, 4))
    )
  }

  glm_coef_tbl <- function(mod) {
    cm <- summary(mod)$coefficients
    ci <- tryCatch(confint(mod), error = function(e) matrix(NA, nrow = nrow(cm), ncol = 2))
    df <- data.frame(
      Term        = rownames(cm),
      Estimate    = round(cm[, 1], 5),
      `Std.Error` = round(cm[, 2], 5),
      `z value`   = round(cm[, 3], 4),
      `Pr(>|z|)`  = vapply(cm[, 4], fmt_p, character(1)),
      `CI 2.5%`   = round(ci[, 1], 5),
      `CI 97.5%`  = round(ci[, 2], 5),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    df
  }

  # ── Results UI ──────────────────────────────────────────────────────────────
  output$glm_results_ui <- renderUI({
    gm <- glm_models()

    model_panels <- list()

    # ── Logit panel ─────────────────────────────────────────────────────────
    if (!is.null(gm$m_logit)) {
      model_panels <- c(model_panels, list(
        div(class = "content-card",
            div(class = "card-header", "📊 Logit Model Summary"),
            div(class = "info-card", HTML(paste0("<strong>Family:</strong> Binomial (logit link)<br><strong>Formula:</strong> ", gm$formula_display))),
            withSpinner(uiOutput("glm_logit_stats"), color = "#6366f1", type = 4),
            div(class = "table-card", h4("Coefficients"), DTOutput("glm_logit_coef")),
            withSpinner(plotOutput("glm_logit_plot", height = "400px"), color = "#6366f1", type = 4),
            div(class = "content-card", div(class = "card-header", "🔮 Logit: Prediction Grid"),
                div(class = "info-card", HTML("Predicted log-odds and probabilities for a grid of the first numeric predictor.")),
                withSpinner(uiOutput("glm_logit_pred_ui"), color = "#6366f1", type = 4),
                div(class = "table-card", DTOutput("glm_logit_pred_tbl")))
        )
      ))
    }

    # ── Probit panel ─────────────────────────────────────────────────────────
    if (!is.null(gm$m_probit)) {
      model_panels <- c(model_panels, list(
        div(class = "content-card",
            div(class = "card-header", "📊 Probit Model Summary"),
            div(class = "info-card", HTML(paste0("<strong>Family:</strong> Binomial (probit link)<br><strong>Formula:</strong> ", gm$formula_display))),
            withSpinner(uiOutput("glm_probit_stats"), color = "#6366f1", type = 4),
            div(class = "table-card", h4("Coefficients"), DTOutput("glm_probit_coef")),
            withSpinner(plotOutput("glm_probit_plot", height = "400px"), color = "#6366f1", type = 4)
        )
      ))
    }

    # ── Comparison panel (both) ───────────────────────────────────────────────
    if (!is.null(gm$m_logit) && !is.null(gm$m_probit)) {
      model_panels <- c(model_panels, list(
        div(class = "content-card",
            div(class = "card-header", "📊 Logit vs Probit — Side-by-Side Fit"),
            div(class = "info-card", HTML("Both curves fitted over the first numeric predictor. Logit has slightly heavier tails than probit.")),
            withSpinner(plotOutput("glm_both_plot", height = "420px"), color = "#6366f1", type = 4))
      ))
    }

    # ── Poisson panel ─────────────────────────────────────────────────────────
    if (!is.null(gm$m_poisson)) {
      model_panels <- c(model_panels, list(
        div(class = "content-card",
            div(class = "card-header", "📊 Poisson Regression Summary"),
            div(class = "info-card", HTML(paste0("<strong>Family:</strong> Poisson (log link)<br><strong>Formula:</strong> ", gm$formula_display))),
            withSpinner(uiOutput("glm_poisson_stats"), color = "#6366f1", type = 4),
            div(class = "table-card", h4("Coefficients"), DTOutput("glm_poisson_coef")),
            withSpinner(plotOutput("glm_poisson_plot", height = "400px"), color = "#6366f1", type = 4),
            div(class = "content-card", div(class = "card-header", "🔮 Poisson: Prediction for New Data"),
                div(class = "info-card", HTML("Predicted counts for a grid of the first numeric predictor.")),
                div(class = "table-card", DTOutput("glm_poisson_pred_tbl")))
        )
      ))
    }

    tagList(model_panels)
  })

  # ── Logit stats ──────────────────────────────────────────────────────────────
  output$glm_logit_stats <- renderUI({
    gm  <- glm_models(); mod <- gm$m_logit; req(mod)
    div(class = "visual-stats",
        div(class = "visual-stat-item", h4("Model Fit & LR Test"), lr_test_ui(mod)),
        div(class = "visual-stat-item", h4("Observations"),
            div(class = "kv",
                div(class = "k", "n"), div(class = "v", nrow(gm$df)),
                div(class = "k", "Predictors"), div(class = "v", length(gm$x_vars))
            ))
    )
  })

  output$glm_logit_coef <- renderDT({
    gm  <- glm_models(); req(gm$m_logit)
    DT::datatable(glm_coef_tbl(gm$m_logit),
                  options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$glm_logit_plot <- renderPlot({
    gm  <- glm_models(); mod <- gm$m_logit; req(mod)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    y    <- gm$df[[gm$y_var]]
    xseq <- seq(min(x), max(x), length.out = 200)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 200), , drop = FALSE]
    nd[[x_num]] <- xseq
    probs <- predict(mod, newdata = nd, type = "response")

    plot(x, y, pch = 19, col = adjustcolor("#3b82f6", 0.6), cex = 0.8,
         xlab = x_num, ylab = paste0("P(", gm$y_var, " = 1)"),
         main = "Logit: Fitted Probability Curve",
         ylim = c(-0.1, 1.2), col.main = "#111827", cex.main = 1.3)
    lines(xseq, probs, col = "#16a34a", lwd = 2.5)
    abline(h = 0.5, lty = 2, col = "#ef4444", lwd = 1.5)
    legend("left", legend = c("Observations", "Fitted P(Y=1)", "P = 0.5"),
           col = c(adjustcolor("#3b82f6", 0.6), "#16a34a", "#ef4444"),
           pch = c(19, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2.5, 1.5), bty = "n")
    grid(col = "gray88")
  })

  output$glm_logit_pred_ui <- renderUI({
    gm <- glm_models(); req(gm$m_logit)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x <- gm$df[[x_num]]
    div(class = "info-card",
        HTML(paste0("Showing predicted log-odds and probability for ",
                    x_num, " at 5 evenly-spaced points from ",
                    round(min(x), 2), " to ", round(max(x), 2), ".")))
  })

  output$glm_logit_pred_tbl <- renderDT({
    gm  <- glm_models(); req(gm$m_logit)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    grid <- seq(min(x), max(x), length.out = 5)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 5), , drop = FALSE]
    nd[[x_num]] <- grid
    log_odds <- predict(gm$m_logit, newdata = nd, type = "link")
    probs    <- predict(gm$m_logit, newdata = nd, type = "response")
    out <- data.frame(
      `X value`  = round(grid, 3),
      `Log-odds` = round(log_odds, 4),
      `P(Y=1)`   = round(probs, 4),
      `Predicted` = ifelse(probs > 0.5, "Yes (1)", "No (0)"),
      check.names = FALSE
    )
    DT::datatable(out, options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
  })

  # ── Probit stats ─────────────────────────────────────────────────────────────
  output$glm_probit_stats <- renderUI({
    gm  <- glm_models(); mod <- gm$m_probit; req(mod)
    div(class = "visual-stats",
        div(class = "visual-stat-item", h4("Model Fit & LR Test"), lr_test_ui(mod)),
        div(class = "visual-stat-item", h4("Observations"),
            div(class = "kv",
                div(class = "k", "n"), div(class = "v", nrow(gm$df)),
                div(class = "k", "Predictors"), div(class = "v", length(gm$x_vars))
            ))
    )
  })

  output$glm_probit_coef <- renderDT({
    gm <- glm_models(); req(gm$m_probit)
    DT::datatable(glm_coef_tbl(gm$m_probit),
                  options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$glm_probit_plot <- renderPlot({
    gm  <- glm_models(); mod <- gm$m_probit; req(mod)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    y    <- gm$df[[gm$y_var]]
    xseq <- seq(min(x), max(x), length.out = 200)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 200), , drop = FALSE]
    nd[[x_num]] <- xseq
    probs <- predict(mod, newdata = nd, type = "response")

    plot(x, y, pch = 19, col = adjustcolor("#3b82f6", 0.6), cex = 0.8,
         xlab = x_num, ylab = paste0("P(", gm$y_var, " = 1)"),
         main = "Probit: Fitted Probability Curve",
         ylim = c(-0.1, 1.2), col.main = "#111827", cex.main = 1.3)
    lines(xseq, probs, col = "#7c3aed", lwd = 2.5)
    abline(h = 0.5, lty = 2, col = "#ef4444", lwd = 1.5)
    legend("left", legend = c("Observations", "Fitted P(Y=1)", "P = 0.5"),
           col = c(adjustcolor("#3b82f6", 0.6), "#7c3aed", "#ef4444"),
           pch = c(19, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2.5, 1.5), bty = "n")
    grid(col = "gray88")
  })

  # ── Logit vs Probit overlay ───────────────────────────────────────────────────
  output$glm_both_plot <- renderPlot({
    gm <- glm_models(); req(gm$m_logit, gm$m_probit)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    y    <- gm$df[[gm$y_var]]
    xseq <- seq(min(x), max(x), length.out = 300)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 300), , drop = FALSE]
    nd[[x_num]] <- xseq

    p_logit  <- predict(gm$m_logit,  newdata = nd, type = "response")
    p_probit <- predict(gm$m_probit, newdata = nd, type = "response")

    plot(x, y, pch = 19, col = adjustcolor("#3b82f6", 0.5), cex = 0.8,
         xlab = x_num, ylab = paste0("P(", gm$y_var, " = 1)"),
         main = "Logit vs Probit: Fitted Probabilities",
         ylim = c(-0.1, 1.2), col.main = "#111827", cex.main = 1.3)
    lines(xseq, p_logit,  col = "#16a34a", lwd = 2.5)
    lines(xseq, p_probit, col = "#7c3aed", lwd = 2.5, lty = 2)
    abline(h = 0.5, lty = 3, col = "#ef4444")
    legend("left", legend = c("Observations", "Logit", "Probit", "P = 0.5"),
           col = c(adjustcolor("#3b82f6", 0.5), "#16a34a", "#7c3aed", "#ef4444"),
           pch = c(19, NA, NA, NA), lty = c(NA, 1, 2, 3), lwd = c(NA, 2.5, 2.5, 1), bty = "n")
    grid(col = "gray88")
  })

  # ── Poisson stats ─────────────────────────────────────────────────────────────
  output$glm_poisson_stats <- renderUI({
    gm  <- glm_models(); mod <- gm$m_poisson; req(mod)
    # Overdispersion check
    phi <- mod$deviance / mod$df.residual
    div(class = "visual-stats",
        div(class = "visual-stat-item", h4("Model Fit & LR Test"), lr_test_ui(mod)),
        div(class = "visual-stat-item", h4("Dispersion"),
            div(class = "kv",
                div(class = "k", "Dispersion φ = D/df"), div(class = "v", round(phi, 4)),
                div(class = "k", "Interpretation"),
                div(class = "v", if (phi > 1.5) "⚠️ Overdispersion (φ > 1.5)" else "✅ Adequate fit (φ ≈ 1)")
            ))
    )
  })

  output$glm_poisson_coef <- renderDT({
    gm <- glm_models(); req(gm$m_poisson)
    DT::datatable(glm_coef_tbl(gm$m_poisson),
                  options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
  })

  output$glm_poisson_plot <- renderPlot({
    gm  <- glm_models(); mod <- gm$m_poisson; req(mod)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    y    <- gm$df[[gm$y_var]]
    xseq <- seq(min(x), max(x), length.out = 200)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 200), , drop = FALSE]
    nd[[x_num]] <- xseq
    mu <- predict(mod, newdata = nd, type = "response")

    plot(x, y, pch = 19, col = adjustcolor("#3b82f6", 0.6), cex = 0.8,
         xlab = x_num, ylab = paste0("E(", gm$y_var, ")"),
         main = "Poisson: Fitted Mean Count",
         col.main = "#111827", cex.main = 1.3)
    lines(xseq, mu, col = "#ef4444", lwd = 2.5)
    legend("topleft", legend = c("Observed counts", "Fitted Poisson mean"),
           col = c(adjustcolor("#3b82f6", 0.6), "#ef4444"),
           pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2.5), bty = "n")
    grid(col = "gray88")
  })

  output$glm_poisson_pred_tbl <- renderDT({
    gm  <- glm_models(); req(gm$m_poisson)
    x_num <- gm$x_vars[sapply(gm$df[gm$x_vars], is.numeric)][1]
    req(!is.na(x_num))
    x    <- gm$df[[x_num]]
    grid <- seq(min(x), max(x), length.out = 5)
    nd   <- gm$df[1, , drop = FALSE]
    nd   <- nd[rep(1, 5), , drop = FALSE]
    nd[[x_num]] <- grid
    log_mu <- predict(gm$m_poisson, newdata = nd, type = "link")
    mu     <- predict(gm$m_poisson, newdata = nd, type = "response")
    out <- data.frame(
      `X value`      = round(grid, 3),
      `Log(μ)`       = round(log_mu, 4),
      `Predicted μ`  = round(mu, 4),
      `Rounded count` = round(mu),
      check.names = FALSE
    )
    DT::datatable(out, options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
  })
}
