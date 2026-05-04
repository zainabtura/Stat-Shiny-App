register_spline_server <- function(input, output, session, state) {

  # ── UI ──────────────────────────────────────────────────────────────────────
  output$spline_ui <- renderUI({
    req(state$data_in())
    nums <- state$numeric_vars()
    validate(need(length(nums) >= 2, "Need at least 2 numeric variables for spline regression."))

    tagList(
      div(
        class = "tests-page-shell",
        div(
          class = "tests-page-hero",
          h2("Spline / Piecewise Regression"),
          p("Fit polynomial and B-spline models with user-defined knot locations.
             Compare linear, quadratic, and cubic splines. Useful when the relationship
             between Y and X changes character across segments of X.")
        )
      ),

      # ── Controls ──────────────────────────────────────────────────────────
      div(
        class = "content-card",
        div(class = "card-header", "⚙️ Variable & Knot Configuration"),
        div(
          class = "tests-input-grid regression-builder-grid",
          div(class = "tests-control",
              selectInput("spl_y", "Response Variable (Y)", choices = nums, selected = nums[1])),
          div(class = "tests-control",
              selectInput("spl_x", "Predictor Variable (X)", choices = nums,
                          selected = if (length(nums) >= 2) nums[2] else nums[1]))
        ),
        div(
          class = "info-card",
          HTML("Enter knot positions separated by commas. Knots must be within the range of X.
                <br><strong>Example:</strong> for one knot at 10 enter <code>10</code>;
                for two knots enter <code>6.5, 13</code>.")
        ),
        div(
          class = "tests-input-grid",
          div(class = "tests-control",
              textInput("spl_knot1", "Knot for 1-knot models (linear spline)", value = "")),
          div(class = "tests-control",
              textInput("spl_knots2", "Knots for 2-knot models (quadratic & cubic)", value = ""))
        ),
        div(
          class = "tests-control regression-build-action",
          actionButton("spl_run", "Fit Spline Models", class = "btn tests-analyze-btn regression-build-btn")
        ),
        uiOutput("spl_range_hint")
      ),

      uiOutput("spl_results_ui")
    )
  })

  # ── Show data range hint ──────────────────────────────────────────────────────
  output$spl_range_hint <- renderUI({
    req(state$data_in(), input$spl_x)
    df <- state$data_in()
    x  <- df[[input$spl_x]]
    x  <- x[is.finite(x)]
    div(class = "info-card success",
        HTML(paste0("X range: <strong>[", round(min(x), 3), ", ", round(max(x), 3), "]</strong>")))
  })

  # ── Reactive: parse knots ─────────────────────────────────────────────────────
  parse_knots <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    k <- suppressWarnings(as.numeric(strsplit(trimws(txt), "[,\\s]+")[[1]]))
    k <- k[!is.na(k)]
    if (length(k) == 0) return(NULL)
    sort(k)
  }

  # ── Reactive: fitted models ───────────────────────────────────────────────────
  spline_models <- eventReactive(input$spl_run, {
    req(state$data_in(), input$spl_y, input$spl_x)
    validate(need(input$spl_y != input$spl_x, "Response and predictor must differ."))

    df   <- state$data_in()
    y    <- df[[input$spl_y]]
    x    <- df[[input$spl_x]]
    keep <- complete.cases(y, x)
    y    <- y[keep]; x <- x[keep]
    validate(need(length(y) >= 8, "Need at least 8 complete observations."))

    xmin <- min(x); xmax <- max(x)
    dat  <- data.frame(y = y, x = x)

    # Polynomial reference models
    m_linear <- lm(y ~ x, data = dat)
    m_quad   <- lm(y ~ x + I(x^2), data = dat)
    m_cubic  <- lm(y ~ x + I(x^2) + I(x^3), data = dat)
    m_quart  <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = dat)

    # Parse knots
    k1  <- parse_knots(input$spl_knot1)
    k2  <- parse_knots(input$spl_knots2)

    # Validate knots within range
    validate_knots <- function(ks, label) {
      if (!is.null(ks)) {
        bad <- ks[ks <= xmin | ks >= xmax]
        if (length(bad) > 0) {
          validate(need(FALSE, paste0(label, ": knot(s) ", paste(bad, collapse = ", "),
                                     " outside X range (", round(xmin,3), ", ", round(xmax,3), ").")))
        }
      }
    }
    validate_knots(k1, "1-knot model")
    validate_knots(k2, "2-knot model")

    spline_fits <- list()
    if (!is.null(k1) && length(k1) == 1) {
      spline_fits$m_lin1 <- tryCatch(
        lm(y ~ splines::bs(x, knots = k1, degree = 1, intercept = TRUE), data = dat),
        error = function(e) NULL)
    }
    if (!is.null(k2) && length(k2) >= 1) {
      spline_fits$m_lin2  <- tryCatch(lm(y ~ splines::bs(x, knots = k2, degree = 1), data = dat), error = function(e) NULL)
      spline_fits$m_quad2 <- tryCatch(lm(y ~ splines::bs(x, knots = k2, degree = 2), data = dat), error = function(e) NULL)
      spline_fits$m_cub2  <- tryCatch(lm(y ~ splines::bs(x, knots = k2, degree = 3), data = dat), error = function(e) NULL)
    }

    list(
      dat          = dat,
      y_var        = input$spl_y,
      x_var        = input$spl_x,
      knot1        = k1,
      knot2        = k2,
      m_linear     = m_linear,
      m_quad       = m_quad,
      m_cubic      = m_cubic,
      m_quart      = m_quart,
      spline_fits  = spline_fits
    )
  })

  # ── Helper: metric row ─────────────────────────────────────────────────────
  spline_metric_row <- function(mod, label) {
    if (is.null(mod)) return(data.frame(Model = label, R2 = NA, Adj_R2 = NA, RSE = NA, MSE = NA, AIC = NA, check.names = FALSE))
    s <- summary(mod)
    data.frame(
      Model  = label,
      R2     = round(s$r.squared, 4),
      Adj_R2 = round(s$adj.r.squared, 4),
      RSE    = round(s$sigma, 4),
      MSE    = round(mean(residuals(mod)^2), 4),
      AIC    = round(AIC(mod), 2),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  # ── Results UI ──────────────────────────────────────────────────────────────
  output$spl_results_ui <- renderUI({
    sm <- spline_models()

    spl_panels <- list()

    if (!is.null(sm$spline_fits$m_lin1)) {
      spl_panels <- c(spl_panels, list(
        div(class = "content-card",
            div(class = "card-header", paste0("〰️ Linear Spline — 1 knot at x = ", sm$knot1)),
            div(class = "info-card", HTML("Piecewise linear fit, continuous at the knot. Fit improves over global linear when the slope changes.")),
            withSpinner(uiOutput("spl_m_lin1_stats"), color = "#6366f1", type = 4),
            fluidRow(
              column(6, withSpinner(plotOutput("spl_m_lin1_scatter", height = "350px"), color = "#6366f1", type = 4)),
              column(6, withSpinner(plotOutput("spl_m_lin1_resid",   height = "350px"), color = "#6366f1", type = 4))
            ))
      ))
    }

    if (!is.null(sm$spline_fits$m_lin2)) {
      spl_panels <- c(spl_panels, list(
        div(class = "content-card",
            div(class = "card-header", paste0("〰️ Linear Spline — 2 knots at x = ", paste(sm$knot2, collapse = ", "))),
            div(class = "info-card", HTML("Piecewise linear fit with two breakpoints. Each segment has its own slope.")),
            withSpinner(uiOutput("spl_m_lin2_stats"), color = "#6366f1", type = 4),
            fluidRow(
              column(6, withSpinner(plotOutput("spl_m_lin2_scatter", height = "350px"), color = "#6366f1", type = 4)),
              column(6, withSpinner(plotOutput("spl_m_lin2_resid",   height = "350px"), color = "#6366f1", type = 4))
            ))
      ))
    }

    if (!is.null(sm$spline_fits$m_quad2)) {
      spl_panels <- c(spl_panels, list(
        div(class = "content-card",
            div(class = "card-header", paste0("〰️ Quadratic Spline — 2 knots at x = ", paste(sm$knot2, collapse = ", "))),
            div(class = "info-card", HTML("Smooth at knots (continuous value and slope). Provides smoother curves than the linear spline.")),
            withSpinner(uiOutput("spl_m_quad2_stats"), color = "#6366f1", type = 4),
            fluidRow(
              column(6, withSpinner(plotOutput("spl_m_quad2_scatter", height = "350px"), color = "#6366f1", type = 4)),
              column(6, withSpinner(plotOutput("spl_m_quad2_resid",   height = "350px"), color = "#6366f1", type = 4))
            ))
      ))
    }

    if (!is.null(sm$spline_fits$m_cub2)) {
      spl_panels <- c(spl_panels, list(
        div(class = "content-card",
            div(class = "card-header", paste0("〰️ Cubic Spline — 2 knots at x = ", paste(sm$knot2, collapse = ", "))),
            div(class = "info-card success", HTML("Cubic splines are smooth at knots in value, slope, and curvature. The most commonly used spline in practice.")),
            withSpinner(uiOutput("spl_m_cub2_stats"), color = "#6366f1", type = 4),
            fluidRow(
              column(6, withSpinner(plotOutput("spl_m_cub2_scatter", height = "350px"), color = "#6366f1", type = 4)),
              column(6, withSpinner(plotOutput("spl_m_cub2_resid",   height = "350px"), color = "#6366f1", type = 4))
            ))
      ))
    }

    tagList(
      # Polynomial reference models
      div(class = "content-card",
          div(class = "card-header", "📊 Reference: Global Polynomial Models"),
          div(class = "info-card", HTML("These global polynomial models serve as baseline comparisons before splines are applied.")),
          fluidRow(
            column(6, withSpinner(plotOutput("spl_ref_scatter", height = "380px"), color = "#6366f1", type = 4)),
            column(6, withSpinner(plotOutput("spl_ref_resid_quart", height = "380px"), color = "#6366f1", type = 4))
          )),

      # Spline model panels
      if (length(spl_panels) > 0) tagList(spl_panels)
      else div(class = "info-card warning",
               HTML("Enter knot positions above and click <strong>Fit Spline Models</strong> to see spline results.")),

      # Comparison table
      div(class = "content-card",
          div(class = "card-header", "📋 All Models Comparison"),
          div(class = "table-card", DTOutput("spl_comparison_tbl")))
    )
  })

  # ── Reference plots ──────────────────────────────────────────────────────────
  output$spl_ref_scatter <- renderPlot({
    sm  <- spline_models()
    dat <- sm$dat
    x   <- dat$x; y <- dat$y
    xseq <- seq(min(x), max(x), length.out = 200)

    plot(x, y, pch = 21, col = "darkblue", bg = adjustcolor("#6366f1", 0.35), cex = 0.85,
         xlab = sm$x_var, ylab = sm$y_var,
         main = "Data + Global Polynomial Fits", col.main = "#111827", cex.main = 1.2)

    mods <- list(
      list(mod = sm$m_linear, col = "#9ca3af", label = "Linear"),
      list(mod = sm$m_quad,   col = "#f59e0b", label = "Quadratic"),
      list(mod = sm$m_cubic,  col = "#6366f1", label = "Cubic"),
      list(mod = sm$m_quart,  col = "#ef4444", label = "Quartic")
    )
    for (m in mods) {
      lines(xseq, predict(m$mod, newdata = data.frame(x = xseq)), col = m$col, lwd = 2)
    }
    legend("topright", legend = sapply(mods, `[[`, "label"),
           col = sapply(mods, `[[`, "col"), lwd = 2, bty = "n")
    grid(col = "gray88")
  })

  output$spl_ref_resid_quart <- renderPlot({
    sm <- spline_models()
    plot(sm$m_quart, which = 1,
         col = adjustcolor("#6366f1", 0.6), pch = 19, cex = 1,
         main = "Residuals vs Fitted (Quartic Model)",
         col.main = "#111827", cex.main = 1.2)
  })

  # ── Generic spline plot/stats helpers ─────────────────────────────────────────
  render_spl_stats <- function(mod_key) {
    renderUI({
      sm  <- spline_models()
      mod <- sm$spline_fits[[mod_key]]
      if (is.null(mod)) return(div(class = "info-card warning", "Model could not be fitted."))
      s  <- summary(mod)
      f  <- s$fstatistic
      pv <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA

      div(class = "visual-stats",
          div(class = "visual-stat-item",
              h4("Model Fit"),
              div(class = "kv",
                  div(class = "k", "R²"),      div(class = "v", round(s$r.squared, 4)),
                  div(class = "k", "Adj. R²"), div(class = "v", round(s$adj.r.squared, 4)),
                  div(class = "k", "RSE"),     div(class = "v", round(s$sigma, 4)),
                  div(class = "k", "MSE"),     div(class = "v", round(mean(residuals(mod)^2), 4)),
                  div(class = "k", "df"),      div(class = "v", mod$df.residual),
                  div(class = "k", "p-value"), div(class = "v", if (!is.na(pv)) fmt_p(pv) else "N/A")
              )
          )
      )
    })
  }

  render_spl_scatter <- function(mod_key) {
    renderPlot({
      sm   <- spline_models()
      mod  <- sm$spline_fits[[mod_key]]
      if (is.null(mod)) { plot.new(); text(0.5, 0.5, "Model unavailable", col = "red"); return() }
      dat  <- sm$dat
      x    <- dat$x; y <- dat$y
      xseq <- seq(min(x), max(x), length.out = 300)

      plot(x, y, pch = 21, col = "darkblue", bg = adjustcolor("#6366f1", 0.35), cex = 0.85,
           xlab = sm$x_var, ylab = sm$y_var,
           main = "Scatter with Spline Fit", col.main = "#111827", cex.main = 1.2)
      lines(xseq, predict(mod, newdata = data.frame(x = xseq)), col = "#ef4444", lwd = 2.5)
      grid(col = "gray88")
    })
  }

  render_spl_resid <- function(mod_key) {
    renderPlot({
      sm  <- spline_models()
      mod <- sm$spline_fits[[mod_key]]
      if (is.null(mod)) { plot.new(); text(0.5, 0.5, "Model unavailable", col = "red"); return() }
      plot(mod, which = 1, col = adjustcolor("#6366f1", 0.6), pch = 19, cex = 1,
           col.main = "#111827", cex.main = 1.2)
    })
  }

  # ── Wire spline panels ────────────────────────────────────────────────────────
  output$spl_m_lin1_stats   <- render_spl_stats("m_lin1")
  output$spl_m_lin2_stats   <- render_spl_stats("m_lin2")
  output$spl_m_quad2_stats  <- render_spl_stats("m_quad2")
  output$spl_m_cub2_stats   <- render_spl_stats("m_cub2")

  output$spl_m_lin1_scatter  <- render_spl_scatter("m_lin1")
  output$spl_m_lin2_scatter  <- render_spl_scatter("m_lin2")
  output$spl_m_quad2_scatter <- render_spl_scatter("m_quad2")
  output$spl_m_cub2_scatter  <- render_spl_scatter("m_cub2")

  output$spl_m_lin1_resid    <- render_spl_resid("m_lin1")
  output$spl_m_lin2_resid    <- render_spl_resid("m_lin2")
  output$spl_m_quad2_resid   <- render_spl_resid("m_quad2")
  output$spl_m_cub2_resid    <- render_spl_resid("m_cub2")

  # ── Comparison table ──────────────────────────────────────────────────────────
  output$spl_comparison_tbl <- renderDT({
    sm <- spline_models()

    rows <- rbind(
      spline_metric_row(sm$m_linear, "Linear (global)"),
      spline_metric_row(sm$m_quad,   "Quadratic (global)"),
      spline_metric_row(sm$m_cubic,  "Cubic (global)"),
      spline_metric_row(sm$m_quart,  "Quartic (global)")
    )

    sf <- sm$spline_fits
    if (!is.null(sf$m_lin1))  rows <- rbind(rows, spline_metric_row(sf$m_lin1,  paste0("Linear Spline (1 knot at ", sm$knot1, ")")))
    if (!is.null(sf$m_lin2))  rows <- rbind(rows, spline_metric_row(sf$m_lin2,  paste0("Linear Spline (2 knots at ", paste(sm$knot2, collapse = ", "), ")")))
    if (!is.null(sf$m_quad2)) rows <- rbind(rows, spline_metric_row(sf$m_quad2, paste0("Quadratic Spline (knots at ", paste(sm$knot2, collapse = ", "), ")")))
    if (!is.null(sf$m_cub2))  rows <- rbind(rows, spline_metric_row(sf$m_cub2,  paste0("Cubic Spline (knots at ", paste(sm$knot2, collapse = ", "), ")")))

    DT::datatable(rows, options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE),
                  class = "cell-border stripe hover", rownames = FALSE)
  })
}
