register_polynomial_server <- function(input, output, session, state) {

  # ── UI ──────────────────────────────────────────────────────────────────────
  output$polynomial_ui <- renderUI({
    req(state$data_in())
    nums <- state$numeric_vars()
    validate(need(length(nums) >= 2,
                  "Need at least 2 numeric variables (1 response, 1 predictor) for polynomial regression."))

    tagList(
      div(
        class = "tests-page-shell",
        div(
          class = "tests-page-hero",
          h2("Polynomial Regression & Centering"),
          p("Fit linear, quadratic, and cubic polynomial models and explore how centering
             the predictor reduces multicollinearity without changing fit quality.")
        )
      ),

      # ── Variable selector ──────────────────────────────────────────────────
      div(
        class = "content-card",
        div(class = "card-header", "⚙️ Variable Selection"),
        div(
          class = "tests-input-grid regression-builder-grid regression-builder-grid-single",
          div(class = "tests-control",
              selectInput("poly_y", "Response Variable (Y)", choices = nums, selected = nums[1])),
          div(class = "tests-control",
              selectInput("poly_x", "Predictor Variable (X)", choices = nums,
                          selected = if (length(nums) >= 2) nums[2] else nums[1])),
          div(class = "tests-control regression-build-action",
              actionButton("poly_run", "Fit Models", class = "btn tests-analyze-btn regression-build-btn"))
        )
      ),

      # ── Results (hidden until run) ─────────────────────────────────────────
      uiOutput("poly_results_ui")
    )
  })

  # ── Reactive: fitted models ──────────────────────────────────────────────────
  poly_models <- eventReactive(input$poly_run, {
    req(state$data_in(), input$poly_y, input$poly_x)
    validate(need(input$poly_y != input$poly_x, "Response and predictor must be different variables."))

    df <- state$data_in()
    y  <- df[[input$poly_y]]
    x  <- df[[input$poly_x]]

    keep <- complete.cases(y, x)
    y <- y[keep]; x <- x[keep]
    validate(need(length(y) >= 6, "Need at least 6 complete observations."))

    xc <- x - mean(x)
    dat <- data.frame(y = y, x = x, xc = xc)

    list(
      dat    = dat,
      y_var  = input$poly_y,
      x_var  = input$poly_x,
      m1     = lm(y ~ x,                   data = dat),   # linear
      m2     = lm(y ~ x  + I(x^2),         data = dat),   # quadratic
      m3     = lm(y ~ xc + I(xc^2),        data = dat),   # quadratic centered
      m4     = lm(y ~ x  + I(x^2) + I(x^3), data = dat), # cubic
      m5     = lm(y ~ xc + I(xc^2) + I(xc^3), data = dat) # cubic centered
    )
  })

  # ── Helper: model metrics row ─────────────────────────────────────────────
  model_metric_row <- function(mod, label) {
    s <- summary(mod)
    data.frame(
      Model    = label,
      R2       = round(s$r.squared, 4),
      Adj_R2   = round(s$adj.r.squared, 4),
      RSE      = round(s$sigma, 4),
      MSE      = round(mean(residuals(mod)^2), 4),
      AIC      = round(AIC(mod), 2),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  safe_vif <- function(mod) {
    tryCatch(car::vif(mod), error = function(e) NULL)
  }

  # ── Results UI ──────────────────────────────────────────────────────────────
  output$poly_results_ui <- renderUI({
    pm <- poly_models()

    tagList(
      # Scatter plot
      div(class = "content-card",
          div(class = "card-header", "📊 Scatter Plot (Raw Data)"),
          withSpinner(plotOutput("poly_scatter", height = "380px"), color = "#6366f1", type = 4)),

      # ── Model 1: Linear ──────────────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📈 Model 1 — Linear Fit"),
          div(class = "info-card", HTML("A straight-line fit. If the data has curvature, you will see a pattern in the residuals plot indicating <strong>lack of fit</strong>.")),
          withSpinner(uiOutput("poly_m1_stats"), color = "#6366f1", type = 4),
          fluidRow(
            column(6, withSpinner(plotOutput("poly_m1_scatter", height = "350px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("poly_m1_resid",   height = "350px"), color = "#6366f1", type = 4))
          )),

      # ── Model 2: Quadratic ────────────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📈 Model 2 — Quadratic Fit (Uncentered)"),
          div(class = "info-card warning", HTML("Quadratic models can have <strong>high multicollinearity</strong> between x and x². Check VIF values below.")),
          withSpinner(uiOutput("poly_m2_stats"), color = "#6366f1", type = 4),
          fluidRow(
            column(6, withSpinner(plotOutput("poly_m2_scatter", height = "350px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("poly_m2_resid",   height = "350px"), color = "#6366f1", type = 4))
          ),
          div(class = "table-card", h4("VIF — Quadratic (Uncentered)"), DTOutput("poly_m2_vif"))),

      # ── Model 3: Quadratic Centered ───────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📈 Model 3 — Quadratic Fit (Centered: xc = x − x̄)"),
          div(class = "info-card success", HTML("Centering xc = x − x̄ <strong>dramatically reduces multicollinearity</strong> while preserving the same fit quality.")),
          withSpinner(uiOutput("poly_m3_stats"), color = "#6366f1", type = 4),
          fluidRow(
            column(6, withSpinner(plotOutput("poly_m3_scatter", height = "350px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("poly_m3_resid",   height = "350px"), color = "#6366f1", type = 4))
          ),
          div(class = "table-card", h4("VIF — Quadratic (Centered)"), DTOutput("poly_m3_vif"))),

      # ── Model 4: Cubic ───────────────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📈 Model 4 — Cubic Fit (Uncentered)"),
          div(class = "info-card warning", HTML("Adding a cubic term can further improve fit but introduces <strong>severe multicollinearity</strong> when uncentered.")),
          withSpinner(uiOutput("poly_m4_stats"), color = "#6366f1", type = 4),
          fluidRow(
            column(6, withSpinner(plotOutput("poly_m4_scatter", height = "350px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("poly_m4_resid",   height = "350px"), color = "#6366f1", type = 4))
          ),
          div(class = "table-card", h4("VIF — Cubic (Uncentered)"), DTOutput("poly_m4_vif"))),

      # ── Model 5: Cubic Centered ──────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📈 Model 5 — Cubic Fit (Centered)"),
          div(class = "info-card success", HTML("Centering the cubic model <strong>reduces multicollinearity</strong> substantially, making coefficient estimates more stable.")),
          withSpinner(uiOutput("poly_m5_stats"), color = "#6366f1", type = 4),
          fluidRow(
            column(6, withSpinner(plotOutput("poly_m5_scatter", height = "350px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("poly_m5_resid",   height = "350px"), color = "#6366f1", type = 4))
          ),
          div(class = "table-card", h4("VIF — Cubic (Centered)"), DTOutput("poly_m5_vif"))),

      # ── VIF comparison bar chart ──────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📊 VIF Comparison Across Models"),
          withSpinner(plotOutput("poly_vif_compare", height = "420px"), color = "#6366f1", type = 4)),

      # ── Model comparison table ────────────────────────────────────────────
      div(class = "content-card",
          div(class = "card-header", "📋 Model Comparison Table"),
          div(class = "info-card", HTML("All five models compared by R², Adj. R², RSE, MSE, and AIC. Fit metrics should be similar for centered vs uncentered versions.")),
          div(class = "table-card", DTOutput("poly_comparison_tbl")))
    )
  })

  # ── Scatter plot ─────────────────────────────────────────────────────────────
  output$poly_scatter <- renderPlot({
    pm <- poly_models()
    with(pm$dat, {
      plot(x, y, pch = 21, col = "darkblue", bg = adjustcolor("#6366f1", 0.4),
           cex = 0.9, xlab = pm$x_var, ylab = pm$y_var,
           main = paste("Scatter Plot:", pm$y_var, "vs", pm$x_var),
           col.main = "#111827", cex.main = 1.3)
      grid(col = "gray88")
    })
  })

  # ── Generic plot/stats helpers ────────────────────────────────────────────────
  render_poly_stats <- function(mod_name) {
    renderUI({
      pm  <- poly_models()
      mod <- pm[[mod_name]]
      s   <- summary(mod)
      f   <- s$fstatistic
      pv  <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA

      div(class = "visual-stats",
          div(class = "visual-stat-item",
              h4("Model Fit"),
              div(class = "kv",
                  div(class = "k", "R²"),     div(class = "v", round(s$r.squared, 4)),
                  div(class = "k", "Adj. R²"), div(class = "v", round(s$adj.r.squared, 4)),
                  div(class = "k", "RSE"),     div(class = "v", round(s$sigma, 4)),
                  div(class = "k", "MSE"),     div(class = "v", round(mean(residuals(mod)^2), 4)),
                  div(class = "k", "F"),        div(class = "v", if (!is.null(f)) round(f[1], 3) else "N/A"),
                  div(class = "k", "p-value"), div(class = "v", if (!is.na(pv)) fmt_p(pv) else "N/A")
              )
          ),
          div(class = "visual-stat-item",
              h4("Coefficients"),
              div(class = "kv",
                  unlist(lapply(names(coef(mod)), function(nm) {
                    list(div(class = "k", nm),
                         div(class = "v", round(coef(mod)[nm], 5)))
                  }), recursive = FALSE)
              )
          )
      )
    })
  }

  # scatter + fitted curve helper
  render_poly_scatter <- function(mod_name, use_centered = FALSE) {
    renderPlot({
      pm  <- poly_models()
      mod <- pm[[mod_name]]
      dat <- pm$dat
      xv  <- if (use_centered) dat$xc else dat$x
      xv_name <- if (use_centered) paste0(pm$x_var, "c (centered)") else pm$x_var

      plot(xv, dat$y, pch = 21, col = "darkblue", bg = adjustcolor("#6366f1", 0.35),
           cex = 0.85, xlab = xv_name, ylab = pm$y_var,
           main = "Scatter with Fitted Curve",
           col.main = "#111827", cex.main = 1.2)

      xseq <- seq(min(xv), max(xv), length.out = 200)
      nd   <- if (use_centered) data.frame(xc = xseq) else data.frame(x = xseq)
      lines(xseq, predict(mod, newdata = nd), col = "#ef4444", lwd = 2)
      grid(col = "gray88")
    })
  }

  render_poly_resid <- function(mod_name) {
    renderPlot({
      pm  <- poly_models()
      mod <- pm[[mod_name]]
      plot(mod, which = 1,
           col  = adjustcolor("#6366f1", 0.6), pch = 19, cex = 1,
           col.main = "#111827", cex.main = 1.2)
    })
  }

  render_poly_vif <- function(mod_name) {
    renderDT({
      pm  <- poly_models()
      mod <- pm[[mod_name]]
      v   <- safe_vif(mod)
      if (is.null(v)) {
        df <- data.frame(Note = "VIF not available (model has only 1 predictor).",
                         stringsAsFactors = FALSE)
      } else {
        df <- data.frame(
          Term   = names(v),
          VIF    = round(as.numeric(v), 4),
          Status = ifelse(v < 5, "✅ Low", ifelse(v < 10, "⚠️ Moderate", "🔴 High")),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(df, options = list(dom = "t", scrollX = TRUE), class = "cell-border stripe hover", rownames = FALSE)
    })
  }

  # ── Wire individual outputs ──────────────────────────────────────────────────
  output$poly_m1_stats   <- render_poly_stats("m1")
  output$poly_m2_stats   <- render_poly_stats("m2")
  output$poly_m3_stats   <- render_poly_stats("m3")
  output$poly_m4_stats   <- render_poly_stats("m4")
  output$poly_m5_stats   <- render_poly_stats("m5")

  output$poly_m1_scatter <- render_poly_scatter("m1", FALSE)
  output$poly_m2_scatter <- render_poly_scatter("m2", FALSE)
  output$poly_m3_scatter <- render_poly_scatter("m3", TRUE)
  output$poly_m4_scatter <- render_poly_scatter("m4", FALSE)
  output$poly_m5_scatter <- render_poly_scatter("m5", TRUE)

  output$poly_m1_resid   <- render_poly_resid("m1")
  output$poly_m2_resid   <- render_poly_resid("m2")
  output$poly_m3_resid   <- render_poly_resid("m3")
  output$poly_m4_resid   <- render_poly_resid("m4")
  output$poly_m5_resid   <- render_poly_resid("m5")

  output$poly_m2_vif     <- render_poly_vif("m2")
  output$poly_m3_vif     <- render_poly_vif("m3")
  output$poly_m4_vif     <- render_poly_vif("m4")
  output$poly_m5_vif     <- render_poly_vif("m5")

  # ── VIF comparison bar chart ──────────────────────────────────────────────────
  output$poly_vif_compare <- renderPlot({
    pm <- poly_models()

    get_max_vif <- function(mod) {
      v <- safe_vif(mod)
      if (is.null(v)) return(1)
      max(v)
    }

    vif_vals <- c(
      "Linear\n(M1)"       = get_max_vif(pm$m1),
      "Quadratic\n(M2)"    = get_max_vif(pm$m2),
      "Quad Cent\n(M3)"    = get_max_vif(pm$m3),
      "Cubic\n(M4)"        = get_max_vif(pm$m4),
      "Cubic Cent\n(M5)"   = get_max_vif(pm$m5)
    )

    colors <- ifelse(vif_vals > 10, "#ef4444",
               ifelse(vif_vals > 5, "#f59e0b", "#22c55e"))

    bp <- barplot(vif_vals,
            col    = colors, border = "white",
            main   = "Max VIF per Model",
            ylab   = "Max VIF",
            ylim   = c(0, max(11, max(vif_vals) * 1.1)),
            las    = 1, cex.names = 0.9,
            col.main = "#111827", cex.main = 1.3)
    abline(h = 5,  lwd = 2, lty = 2, col = "#f59e0b")
    abline(h = 10, lwd = 2, lty = 2, col = "#ef4444")
    legend("topright",
           legend = c("Low (< 5)", "Moderate (5–10)", "High (> 10)", "VIF = 5", "VIF = 10"),
           fill   = c("#22c55e", "#f59e0b", "#ef4444", NA, NA),
           border = NA,
           lty    = c(NA, NA, NA, 2, 2),
           col    = c(NA, NA, NA, "#f59e0b", "#ef4444"),
           lwd    = c(NA, NA, NA, 2, 2),
           bty    = "n", cex = 0.85)
    grid(col = "gray88")
  })

  # ── Comparison table ──────────────────────────────────────────────────────────
  output$poly_comparison_tbl <- renderDT({
    pm <- poly_models()
    tbl <- rbind(
      model_metric_row(pm$m1, "Model 1: Linear"),
      model_metric_row(pm$m2, "Model 2: Quadratic (uncentered)"),
      model_metric_row(pm$m3, "Model 3: Quadratic (centered)"),
      model_metric_row(pm$m4, "Model 4: Cubic (uncentered)"),
      model_metric_row(pm$m5, "Model 5: Cubic (centered)")
    )
    DT::datatable(tbl, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE),
                  class = "cell-border stripe hover", rownames = FALSE)
  })
}
