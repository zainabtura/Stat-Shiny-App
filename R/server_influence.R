register_influence_server <- function(input, output, session, state) {

  influence_model <- reactive({
    built <- state$built_regression()
    validate(need(!is.null(built), "Build a regression model first on the Regression page."))
    built
  })

  # ── UI ───────────────────────────────────────────────────────────────────────
  output$influence_ui <- renderUI({
    built <- state$built_regression()
    if (is.null(built)) {
      return(div(class = "content-card",
        div(class = "card-header", "Influence Diagnostics"),
        div(class = "info-card",
          HTML("Build a regression model first on the <strong>Regression</strong> page."))))
    }
    tagList(
      div(class = "tests-page-shell",
        div(class = "tests-page-hero",
          h2("Diagnostics for Leverage and Influence"),
          p("Identify influential observations using leverage, Cook's D, DFFITS, and robust regression.")
        )
      ),
      div(class = "content-card",
        div(class = "card-header", "Current Built Model"),
        div(class = "info-card", HTML(paste0(
          "<strong>Mode:</strong> ", built$mode, "<br>",
          "<strong>Formula:</strong> ", built$formula_display, "<br>",
          "<strong>n observations:</strong> ", nrow(built$df)
        )))
      ),
      div(class = "content-card",
        div(class = "card-header", "1. Full Influence Measures Table"),
        div(class = "info-card", HTML("Flagged rows (*) exceed standard thresholds.")),
        withSpinner(uiOutput("influence_flag_summary"), color = "#6366f1", type = 4),
        div(class = "table-card", DTOutput("influence_measures_tbl"))
      ),
      div(class = "content-card",
        div(class = "card-header", "2. Diagnostic Plots"),
        fluidRow(
          column(6, withSpinner(plotOutput("influence_leverage_plot", height = "380px"), color = "#6366f1", type = 4)),
          column(6, withSpinner(plotOutput("influence_cooksd_plot",   height = "380px"), color = "#6366f1", type = 4))
        ),
        fluidRow(
          column(6, withSpinner(plotOutput("influence_dffits_plot",   height = "380px"), color = "#6366f1", type = 4)),
          column(6, withSpinner(plotOutput("influence_residlev_plot", height = "380px"), color = "#6366f1", type = 4))
        )
      ),
      div(class = "content-card",
        div(class = "card-header", "3. Model After Removing Influential Observations"),
        div(class = "info-card", HTML("Flagged observations removed and model refit.")),
        withSpinner(uiOutput("influence_model2_summary"), color = "#6366f1", type = 4)
      ),
      div(class = "content-card",
        div(class = "card-header", "4. Robust Regression - Huber's Method"),
        div(class = "info-card", HTML("Observations with large residuals receive down-weighted influence.")),
        withSpinner(uiOutput("influence_huber_summary"), color = "#6366f1", type = 4),
        div(class = "table-card", h4("10 Lowest-Weighted Observations"),
            DTOutput("influence_huber_weights_tbl")),
        withSpinner(plotOutput("influence_huber_plot", height = "400px"), color = "#6366f1", type = 4)
      ),
      div(class = "content-card",
        div(class = "card-header", "5. Robust Regression - Bisquare (Tukey) Method"),
        div(class = "info-card", HTML("Can assign weight 0 to extreme outliers.")),
        withSpinner(uiOutput("influence_bisquare_summary"), color = "#6366f1", type = 4),
        div(class = "table-card", h4("10 Lowest-Weighted Observations"),
            DTOutput("influence_bisquare_weights_tbl")),
        withSpinner(plotOutput("influence_bisquare_plot", height = "400px"), color = "#6366f1", type = 4)
      ),
      div(class = "content-card",
        div(class = "card-header", "6. Four-Model Coefficient & MSE Comparison"),
        div(class = "info-card", HTML(
          "<strong>Model 1</strong>: Full OLS &nbsp;|&nbsp;
           <strong>Model 2</strong>: Influential obs removed &nbsp;|&nbsp;
           <strong>Model 3</strong>: Huber robust &nbsp;|&nbsp;
           <strong>Model 4</strong>: Bisquare robust")),
        withSpinner(uiOutput("influence_comparison_summary"), color = "#6366f1", type = 4),
        div(class = "table-card", DTOutput("influence_comparison_tbl"))
      )
    )
  })

  # ── Influence data frame (hat, cook, dffits) ──────────────────────────────────
  infl_df <- reactive({
    bm <- influence_model()
    n  <- nrow(bm$df)
    p  <- length(coef(bm$model))

    h     <- tryCatch(as.numeric(hatvalues(bm$model)),      error = function(e) rep(NA_real_, n))
    cookd <- tryCatch(as.numeric(cooks.distance(bm$model)), error = function(e) rep(NA_real_, n))
    dff   <- tryCatch(as.numeric(dffits(bm$model)),         error = function(e) rep(NA_real_, n))

    hat_cut    <- 2 * p / n
    cookd_cut  <- 4 / n
    dffits_cut <- 2 * sqrt(p / n)

    flagged <- (!is.na(h)     & h     > hat_cut)    |
               (!is.na(cookd) & cookd > cookd_cut)  |
               (!is.na(dff)   & abs(dff) > dffits_cut)

    data.frame(
      obs        = seq_len(n),
      hat        = h,
      cook.d     = cookd,
      dffits     = dff,
      flagged    = flagged,
      flag_label = ifelse(flagged, "*", ""),
      stringsAsFactors = FALSE
    )
  })

  flagged_idx <- reactive({
    df <- infl_df()
    df$obs[!is.na(df$flagged) & df$flagged]
  })

  # ── Flag banner ───────────────────────────────────────────────────────────────
  output$influence_flag_summary <- renderUI({
    df  <- infl_df()
    bm  <- influence_model()
    n   <- nrow(bm$df)
    p   <- length(coef(bm$model))
    nfl <- sum(df$flagged, na.rm = TRUE)
    cls <- if (nfl == 0) "info-card success" else "info-card warning"
    div(class = cls, HTML(paste0(
      "<strong>Flagged: ", nfl, " of ", n, " observations</strong><br>",
      "Leverage h > ", round(2*p/n, 4),
      " | Cook's D > ", round(4/n, 4),
      " | |DFFITS| > ", round(2*sqrt(p/n), 4),
      if (nfl > 0) paste0("<br><strong>Row indices:</strong> ",
                          paste(flagged_idx(), collapse = ", ")) else ""
    )))
  })

  # ── Measures table ────────────────────────────────────────────────────────────
  output$influence_measures_tbl <- renderDT({
    df   <- infl_df()
    disp <- data.frame(
      Obs        = df$obs,
      Flag       = df$flag_label,
      `Leverage h` = round(df$hat,    5),
      `Cook's D`   = round(df$cook.d, 5),
      DFFITS       = round(df$dffits, 5),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(disp,
      options  = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE),
      class    = "cell-border stripe hover",
      rownames = FALSE)
  })

  # ── Diagnostic plots ──────────────────────────────────────────────────────────
  flag_col <- function(flagged) ifelse(flagged, "#ef4444", adjustcolor("#6366f1", 0.6))

  output$influence_leverage_plot <- renderPlot({
    df  <- infl_df()
    bm  <- influence_model()
    n   <- nrow(bm$df); p <- length(coef(bm$model))
    cut <- 2 * p / n
    validate(need(any(!is.na(df$hat)), "Leverage could not be computed."))
    plot(df$obs, df$hat, xlab = "Observation", ylab = "Leverage (h)",
         main = "Leverage Values", pch = 19, col = flag_col(df$flagged),
         cex = 1.1, col.main = "#111827", cex.main = 1.3)
    abline(h = cut, col = "#f59e0b", lwd = 2, lty = 2)
    legend("topright", legend = c("Normal","Flagged", paste0("h=",round(cut,4))),
           col = c(adjustcolor("#6366f1",0.6),"#ef4444","#f59e0b"),
           pch = c(19,19,NA), lty = c(NA,NA,2), lwd = c(NA,NA,2), bty = "n")
    grid(col = "gray88")
  })

  output$influence_cooksd_plot <- renderPlot({
    df  <- infl_df()
    bm  <- influence_model()
    n   <- nrow(bm$df)
    cut <- 4 / n
    validate(need(any(!is.na(df$cook.d)), "Cook's D could not be computed."))
    cookd <- ifelse(is.na(df$cook.d), 0, df$cook.d)
    barplot(cookd, names.arg = df$obs, col = flag_col(df$flagged),
            main = "Cook's Distance", xlab = "Observation", ylab = "Cook's D",
            border = NA, las = 1, col.main = "#111827", cex.main = 1.3)
    abline(h = cut, col = "#f59e0b", lwd = 2, lty = 2)
    legend("topright", legend = c("Normal","Flagged",paste0("D=",round(cut,4))),
           fill = c(adjustcolor("#6366f1",0.6),"#ef4444",NA),
           border = NA, lty = c(NA,NA,2), col = c(NA,NA,"#f59e0b"),
           lwd = c(NA,NA,2), bty = "n")
  })

  output$influence_dffits_plot <- renderPlot({
    df  <- infl_df()
    bm  <- influence_model()
    n   <- nrow(bm$df); p <- length(coef(bm$model))
    cut <- 2 * sqrt(p / n)
    validate(need(any(!is.na(df$dffits)), "DFFITS could not be computed."))
    plot(df$obs, df$dffits, xlab = "Observation", ylab = "DFFITS",
         main = "DFFITS", pch = 19, col = flag_col(df$flagged),
         cex = 1.1, col.main = "#111827", cex.main = 1.3)
    abline(h =  cut, col = "#f59e0b", lwd = 2, lty = 2)
    abline(h = -cut, col = "#f59e0b", lwd = 2, lty = 2)
    abline(h = 0,    col = "gray60",  lwd = 1)
    legend("topright", legend = c("Normal","Flagged",paste0("+-",round(cut,4))),
           col = c(adjustcolor("#6366f1",0.6),"#ef4444","#f59e0b"),
           pch = c(19,19,NA), lty = c(NA,NA,2), lwd = c(NA,NA,2), bty = "n")
    grid(col = "gray88")
  })

  output$influence_residlev_plot <- renderPlot({
    bm <- influence_model()
    plot(bm$model, which = 5,
         col = adjustcolor("#6366f1", 0.6), pch = 19, cex = 1.1,
         col.main = "#111827", cex.main = 1.3)
  })

  # ── Model 2: remove influential obs ──────────────────────────────────────────
  model2_fit <- reactive({
    bm  <- influence_model()
    bad <- flagged_idx()
    validate(need(length(bad) > 0, "No influential observations flagged."))
    validate(need(
      length(bad) < nrow(bm$df) - length(coef(bm$model)),
      "Too many flagged observations to refit."))

    df2 <- bm$df[-bad, , drop = FALSE]

    # Use safe column names to avoid formula issues with special characters
    df_num  <- df2[, sapply(df2, is.numeric), drop = FALSE]
    y_var   <- bm$y_var
    x_vars  <- bm$x_vars[bm$x_vars %in% names(df_num)]

    # Fall back to original df if we have factors
    df_use  <- df2[, c(y_var, x_vars), drop = FALSE]
    safe_nm <- make.names(names(df_use), unique = TRUE)
    names(df_use) <- safe_nm

    fml <- as.formula(paste0(safe_nm[1], " ~ ",
                             paste0(safe_nm[-1], collapse = " + ")))
    lm(fml, data = df_use)
  })

  output$influence_model2_summary <- renderUI({
    bm  <- influence_model()
    bad <- flagged_idx()
    if (length(bad) == 0)
      return(div(class="info-card success", HTML("No influential observations detected.")))
    m2 <- tryCatch(model2_fit(), error = function(e) NULL)
    if (is.null(m2))
      return(div(class="info-card warning", HTML("Could not refit after removing flagged observations.")))
    m1 <- bm$model; s1 <- summary(m1); s2 <- summary(m2)
    div(class = "visual-stats",
      div(class="visual-stat-item", h4("Model 1 (Full OLS)"),
        div(class="kv",
          div(class="k","n"),       div(class="v", length(residuals(m1))),
          div(class="k","R2"),      div(class="v", round(s1$r.squared, 4)),
          div(class="k","Adj R2"),  div(class="v", round(s1$adj.r.squared, 4)),
          div(class="k","MSE"),     div(class="v", round(mean(residuals(m1)^2), 4)),
          div(class="k","RSE"),     div(class="v", round(s1$sigma, 4))
        )
      ),
      div(class="visual-stat-item", h4(paste0("Model 2 (-", length(bad), " obs)")),
        div(class="kv",
          div(class="k","n"),       div(class="v", length(residuals(m2))),
          div(class="k","R2"),      div(class="v", round(s2$r.squared, 4)),
          div(class="k","Adj R2"),  div(class="v", round(s2$adj.r.squared, 4)),
          div(class="k","MSE"),     div(class="v", round(mean(residuals(m2)^2), 4)),
          div(class="k","RSE"),     div(class="v", round(s2$sigma, 4))
        )
      )
    )
  })

  # ── Build rlm model using ONLY numeric columns with SAFE names ───────────────
  build_rlm <- function(psi_fn) {
    bm  <- influence_model()

    # Keep only numeric columns
    df_num <- bm$df[, sapply(bm$df, is.numeric), drop = FALSE]

    y_var  <- bm$y_var
    x_vars <- bm$x_vars[bm$x_vars %in% names(df_num)]

    validate(need(length(x_vars) >= 1,
      "Robust regression requires numeric predictors."))
    validate(need(y_var %in% names(df_num),
      "Response variable must be numeric for robust regression."))

    df_use <- df_num[, c(y_var, x_vars), drop = FALSE]
    df_use <- df_use[complete.cases(df_use), , drop = FALSE]

    validate(need(nrow(df_use) >= length(x_vars) + 2,
      "Not enough complete observations for robust regression."))

    # ── Rename ALL columns to safe names to avoid formula parsing issues ──────
    # Column names with spaces, commas, parentheses break rlm() internally
    safe_names <- make.names(names(df_use), unique = TRUE)
    names(df_use) <- safe_names

    safe_y    <- safe_names[1]
    safe_x    <- safe_names[-1]

    fml <- as.formula(paste0(safe_y, " ~ ",
                             paste0(safe_x, collapse = " + ")))

    MASS::rlm(fml, data = df_use, psi = psi_fn)
  }

  huber_fit <- reactive({
    tryCatch(
      build_rlm(MASS::psi.huber),
      error = function(e) {
        validate(need(FALSE, paste("Huber robust regression failed:", e$message)))
      }
    )
  })

  bisquare_fit <- reactive({
    tryCatch(
      build_rlm(MASS::psi.bisquare),
      error = function(e) {
        validate(need(FALSE, paste("Bisquare robust regression failed:", e$message)))
      }
    )
  })

  # ── Huber outputs ─────────────────────────────────────────────────────────────
  output$influence_huber_summary <- renderUI({
    mod <- huber_fit()
    cf  <- tryCatch(coef(mod), error = function(e) numeric(0))
    div(class="visual-stats",
      div(class="visual-stat-item", h4("Huber Robust Fit"),
        div(class="kv",
          div(class="k","Scale"),      div(class="v", round(mod$s, 4)),
          div(class="k","Converged"),  div(class="v", if (isTRUE(mod$converged)) "Yes" else "No"),
          div(class="k","Iterations"), div(class="v", mod$iter)
        )
      ),
      div(class="visual-stat-item", h4("Coefficients"),
        div(class="kv",
          unlist(lapply(seq_along(cf), function(i)
            list(div(class="k", names(cf)[i]),
                 div(class="v", round(cf[i], 5)))), recursive = FALSE)
        )
      )
    )
  })

  output$influence_huber_weights_tbl <- renderDT({
    mod <- huber_fit()
    w   <- tryCatch(as.numeric(mod$w),     error = function(e) rep(NA_real_, nrow(mod$model)))
    res <- tryCatch(as.numeric(mod$resid), error = function(e) rep(NA_real_, nrow(mod$model)))
    hw  <- data.frame(Obs = seq_along(w), Residual = round(res, 5), Weight = round(w, 5))
    hw  <- hw[order(hw$Weight), ]
    DT::datatable(head(hw, 10), options = list(dom="t", scrollX=TRUE),
                  class="cell-border stripe hover", rownames=FALSE)
  })

  output$influence_huber_plot <- renderPlot({
    mod <- huber_fit()
    res <- tryCatch(as.numeric(mod$resid), error = function(e) rep(NA_real_, nrow(mod$model)))
    w   <- tryCatch(as.numeric(mod$w),     error = function(e) rep(NA_real_, nrow(mod$model)))
    d   <- data.frame(resid = res, weight = w)
    d   <- d[is.finite(d$resid) & is.finite(d$weight), ]
    validate(need(nrow(d) > 0, "No valid data to plot."))
    plot(d$resid, d$weight, main = "Huber - Residuals vs Weights",
         xlab = "Residuals", ylab = "Weights",
         pch = 19, col = adjustcolor("#16a34a", 0.65), cex = 1.1,
         col.main = "#111827", cex.main = 1.3)
    abline(v = 0, col = "#ef4444", lty = 2, lwd = 2)
    grid(col = "gray88")
  })

  # ── Bisquare outputs ──────────────────────────────────────────────────────────
  output$influence_bisquare_summary <- renderUI({
    mod <- bisquare_fit()
    cf  <- tryCatch(coef(mod), error = function(e) numeric(0))
    div(class="visual-stats",
      div(class="visual-stat-item", h4("Bisquare Robust Fit"),
        div(class="kv",
          div(class="k","Scale"),      div(class="v", round(mod$s, 4)),
          div(class="k","Converged"),  div(class="v", if (isTRUE(mod$converged)) "Yes" else "No"),
          div(class="k","Iterations"), div(class="v", mod$iter)
        )
      ),
      div(class="visual-stat-item", h4("Coefficients"),
        div(class="kv",
          unlist(lapply(seq_along(cf), function(i)
            list(div(class="k", names(cf)[i]),
                 div(class="v", round(cf[i], 5)))), recursive = FALSE)
        )
      )
    )
  })

  output$influence_bisquare_weights_tbl <- renderDT({
    mod <- bisquare_fit()
    w   <- tryCatch(as.numeric(mod$w),     error = function(e) rep(NA_real_, nrow(mod$model)))
    res <- tryCatch(as.numeric(mod$resid), error = function(e) rep(NA_real_, nrow(mod$model)))
    hw  <- data.frame(Obs = seq_along(w), Residual = round(res, 5), Weight = round(w, 5))
    hw  <- hw[order(hw$Weight), ]
    DT::datatable(head(hw, 10), options = list(dom="t", scrollX=TRUE),
                  class="cell-border stripe hover", rownames=FALSE)
  })

  output$influence_bisquare_plot <- renderPlot({
    mod <- bisquare_fit()
    res <- tryCatch(as.numeric(mod$resid), error = function(e) rep(NA_real_, nrow(mod$model)))
    w   <- tryCatch(as.numeric(mod$w),     error = function(e) rep(NA_real_, nrow(mod$model)))
    d   <- data.frame(resid = res, weight = w)
    d   <- d[is.finite(d$resid) & is.finite(d$weight), ]
    validate(need(nrow(d) > 0, "No valid data to plot."))
    plot(d$resid, d$weight, main = "Bisquare - Residuals vs Weights",
         xlab = "Residuals", ylab = "Weights",
         pch = 19, col = adjustcolor("#ea580c", 0.65), cex = 1.1,
         col.main = "#111827", cex.main = 1.3)
    abline(v = 0, col = "#ef4444", lty = 2, lwd = 2)
    grid(col = "gray88")
  })

  # ── Four-model comparison ─────────────────────────────────────────────────────
  build_comparison_row <- function(mod, label, is_rlm = FALSE) {
    cf  <- tryCatch(coef(mod), error = function(e) numeric(0))
    mse <- tryCatch(
      if (is_rlm) as.numeric(mod$s)^2 else mean(as.numeric(residuals(mod))^2),
      error = function(e) NA_real_
    )
    # Build row as a plain data.frame column by column
    df <- data.frame(Model = label, MSE = round(mse, 5),
                     stringsAsFactors = FALSE)
    if (length(cf) > 0) {
      nm <- names(cf)
      if (is.null(nm)) nm <- paste0("Coef", seq_along(cf))
      # Sanitise names for data.frame
      nm_safe <- make.names(nm, unique = TRUE)
      for (i in seq_along(cf)) {
        df[[nm_safe[i]]] <- round(as.numeric(cf[i]), 5)
      }
    }
    df
  }

  output$influence_comparison_tbl <- renderDT({
    bm  <- influence_model()
    m1  <- bm$model
    m3  <- huber_fit()
    m4  <- bisquare_fit()
    bad <- flagged_idx()
    m2  <- tryCatch(if (length(bad) > 0) model2_fit() else m1, error = function(e) m1)

    r1 <- build_comparison_row(m1, "Model 1: Full OLS")
    r2 <- build_comparison_row(m2, if (length(bad) > 0)
                                     paste0("Model 2: OLS (-", length(bad), " obs)")
                                   else "Model 2: OLS (same)")
    r3 <- build_comparison_row(m3, "Model 3: Huber",    is_rlm = TRUE)
    r4 <- build_comparison_row(m4, "Model 4: Bisquare", is_rlm = TRUE)

    all_cols <- unique(c(names(r1), names(r2), names(r3), names(r4)))
    pad <- function(df) {
      for (col in setdiff(all_cols, names(df))) df[[col]] <- NA_real_
      df[, all_cols, drop = FALSE]
    }
    tbl <- do.call(rbind, lapply(list(r1, r2, r3, r4), pad))
    coef_cols <- setdiff(names(tbl), c("Model","MSE"))
    tbl <- tbl[, c("Model","MSE", coef_cols), drop = FALSE]

    DT::datatable(tbl,
      options  = list(dom = "t", scrollX = TRUE, autoWidth = FALSE),
      class    = "cell-border stripe hover",
      rownames = FALSE)
  })

  output$influence_comparison_summary <- renderUI({
    bm  <- influence_model()
    m1  <- bm$model
    m3  <- huber_fit()
    m4  <- bisquare_fit()
    bad <- flagged_idx()
    m2  <- tryCatch(if (length(bad) > 0) model2_fit() else m1, error = function(e) m1)

    mse1 <- tryCatch(mean(residuals(m1)^2), error = function(e) NA_real_)
    mse2 <- tryCatch(mean(residuals(m2)^2), error = function(e) NA_real_)
    mse3 <- tryCatch(as.numeric(m3$s)^2,   error = function(e) NA_real_)
    mse4 <- tryCatch(as.numeric(m4$s)^2,   error = function(e) NA_real_)

    mses <- c(mse1, mse2, mse3, mse4)
    lbls <- c("Model 1 (Full OLS)",
              paste0("Model 2 (-", length(bad), " obs)"),
              "Model 3 (Huber)",
              "Model 4 (Bisquare)")
    best <- which.min(mses)

    div(class = "info-card success", HTML(paste0(
      "<strong>Lowest MSE:</strong> ", lbls[best],
      " (MSE = ", round(mses[best], 4), ")"
    )))
  })
}
