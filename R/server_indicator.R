register_indicator_server <- function(input, output, session, state) {

  # ── Shared helper: build lm and metadata ─────────────────────────────────
  build_ind_model <- function(df, y_var, x_var, cat_vars, interaction) {
    for (v in cat_vars) df[[v]] <- as.factor(df[[v]])
    keep <- complete.cases(df[, c(y_var, x_var, cat_vars)])
    df   <- df[keep, ]

    if (nrow(df) < 4) stop("Not enough complete observations (need at least 4).")

    for (v in cat_vars) {
      n <- length(levels(df[[v]]))
      if (n < 2)  stop(paste0("'", v, "' must have at least 2 levels."))
      if (n > 20) stop(paste0("'", v, "' has too many levels (max 20)."))
    }

    rhs <- if (interaction && length(cat_vars) == 1) {
      paste0("`", x_var, "` * `", cat_vars[1], "`")
    } else {
      paste(c(paste0("`", x_var, "`"), paste0("`", cat_vars, "`")), collapse = " + ")
    }
    f <- as.formula(paste0("`", y_var, "` ~ ", rhs))
    m <- lm(f, data = df)

    list(model = m, df = df, y_var = y_var, x_var = x_var,
         cat_vars = cat_vars, interaction = interaction,
         ref = sapply(cat_vars, function(v) levels(df[[v]])[1]))
  }

  # ── Reactive for the active case ──────────────────────────────────────────
  active_case <- reactive({
    req(input$ind_case); input$ind_case
  })

  ind_model <- reactive({
    req(state$data_in(), input$ind_y, input$ind_x)
    df    <- state$data_in()
    y_var <- input$ind_y
    x_var <- input$ind_x

    validate(
      need(y_var != x_var,          "Response and predictor must differ."),
      need(is.numeric(df[[y_var]]), "Response must be numeric."),
      need(is.numeric(df[[x_var]]), "Quantitative predictor must be numeric.")
    )

    case <- active_case()

    if (case == "two") {
      req(input$ind_cat1)
      d <- input$ind_cat1
      validate(need(is_categorical(df[[d]]), "Indicator variable must be categorical."))
      tryCatch(build_ind_model(df, y_var, x_var, d, isTRUE(input$ind_interaction)),
               error = function(e) { validate(need(FALSE, e$message)) })

    } else if (case == "multi") {
      req(input$ind_cat1)
      d <- input$ind_cat1
      validate(need(is_categorical(df[[d]]), "Indicator variable must be categorical."))
      tryCatch(build_ind_model(df, y_var, x_var, d, FALSE),
               error = function(e) { validate(need(FALSE, e$message)) })

    } else {  # two_vars
      req(input$ind_cat1, input$ind_cat2)
      d1 <- input$ind_cat1; d2 <- input$ind_cat2
      validate(
        need(d1 != d2,                  "Select two different categorical variables."),
        need(is_categorical(df[[d1]]),  "First indicator must be categorical."),
        need(is_categorical(df[[d2]]),  "Second indicator must be categorical.")
      )
      tryCatch(build_ind_model(df, y_var, x_var, c(d1, d2), FALSE),
               error = function(e) { validate(need(FALSE, e$message)) })
    }
  })

  # ── Main UI ───────────────────────────────────────────────────────────────
  output$indicator_ui <- renderUI({
    req(state$data_in())
    nums <- state$numeric_vars()
    cats <- state$categorical_vars()

    if (length(nums) < 2 || length(cats) < 1) {
      missing_bits <- c()
      if (length(nums) < 2) {
        missing_bits <- c(missing_bits, "at least two numeric variables")
      }
      if (length(cats) < 1) {
        missing_bits <- c(missing_bits, "at least one categorical variable")
      }

      return(
        div(
          class = "tests-page-shell",
          div(
            class = "tests-page-hero",
            h2("Indicator Variables"),
            p("Encode categorical predictors as dummy variables and include them in a regression model. Select a case below: one categorical variable with two levels, one with more than two levels, or two separate indicator variables in the same model.")
          ),
          div(
            class = "content-card indicator-empty-state-panel",
            div(class = "indicator-empty-state-kicker", "Indicator Variables"),
            h3("This dataset is not ready for indicator-variable modeling yet."),
            p(
              paste0(
                "Indicator-variable regression needs ",
                paste(missing_bits, collapse = " and "),
                "."
              )
            ),
            div(
              class = "indicator-empty-state-list",
              div(class = "indicator-empty-state-item", strong("What to check"), span("Upload a dataset that includes one numeric response, one numeric predictor, and at least one categorical field.")),
              div(class = "indicator-empty-state-item", strong("Helpful tip"), span("Text columns usually become categorical when they are treated as categories, so they can be used as indicator variables here."))
            )
          )
        )
      )
    }

    case <- if (!is.null(input$ind_case)) input$ind_case else "two"
    indicator_scope_class <- if (identical(case, "two")) {
      "inferential-scope-one"
    } else if (identical(case, "multi")) {
      "inferential-scope-two"
    } else {
      "inferential-scope-three"
    }
    # Default selections
    y_sel  <- if (!is.null(input$ind_y)   && input$ind_y   %in% nums) input$ind_y   else nums[1]
    x_sel  <- if (!is.null(input$ind_x)   && input$ind_x   %in% nums) input$ind_x   else if (length(nums)>=2) nums[2] else nums[1]
    c1_sel <- if (!is.null(input$ind_cat1) && input$ind_cat1 %in% cats) input$ind_cat1 else cats[1]
    c2_sel <- if (!is.null(input$ind_cat2) && input$ind_cat2 %in% cats) input$ind_cat2 else if (length(cats)>=2) cats[2] else cats[1]

    div(
      class = "tests-page-shell",
      # ── Header ──────────────────────────────────────────────────────
      div(
        class = "tests-page-hero",
        h2("Indicator Variables"),
        p("Encode categorical predictors as dummy variables and include them in a regression model. Select a case below: one categorical variable with two levels, one with more than two levels, or two separate indicator variables in the same model.")
      ),

      # ── Case tabs ───────────────────────────────────────────────────
      div(
        class = paste("inferential-tab-shell", indicator_scope_class),
        div(
          class = "inferential-tab-row",
          actionButton(
            "ind_tab_two",
            span(class = "action-label inferential-tab-title", "Indicator Variable with Two Levels"),
            class = paste("btn inferential-tab-button", if (case == "two") "is-active" else ""),
            style = "width: auto;"
          ),
          actionButton(
            "ind_tab_multi",
            span(class = "action-label inferential-tab-title", "Indicator Variable with More Than Two Levels"),
            class = paste("btn inferential-tab-button", if (case == "multi") "is-active" else ""),
            style = "width: auto;"
          ),
          actionButton(
            "ind_tab_two_vars",
            span(class = "action-label inferential-tab-title", "More Than Two Indicator Variables"),
            class = paste("btn inferential-tab-button", if (case == "two_vars") "is-active" else ""),
            style = "width: auto;"
          )
        )
      ),

      div(
        class = "inferential-results-shell",

        # ── Controls ────────────────────────────────────────────────────
        div(
          class = "tests-panel tests-input-panel",
          div(class = "tests-section-title", "Variable Selection"),
          div(
            class = paste(
              "tests-input-grid",
              "indicator-input-grid",
              if (case == "two_vars") "indicator-input-grid-two-vars" else "indicator-input-grid-standard"
            ),

            # Y
            div(
              class = "tests-control",
              selectInput("ind_y", "Response (Y)", choices = nums, selected = y_sel)
            ),
            # X
            div(
              class = "tests-control",
              selectInput("ind_x", "Predictor (X)", choices = nums, selected = x_sel)
            ),
            # Cat 1
            div(
              class = "tests-control",
              selectInput(
                "ind_cat1",
                if (case == "two_vars") "1st Indicator Variable" else "Indicator Variable",
                choices = cats,
                selected = c1_sel
              )
            ),
            # Cat 2 (only for two_vars)
            if (case == "two_vars")
              div(
                class = "tests-control",
                selectInput("ind_cat2", "2nd Indicator Variable", choices = cats, selected = c2_sel)
              ),
            # Interaction toggle (only for two-level)
            if (case == "two")
              div(
                class = "tests-control tests-control-checkbox indicator-model-toggle",
                tags$label("Model Type"),
                div(
                  class = "checkbox",
                  tags$label(
                    `for` = "ind_interaction",
                    tags$input(id="ind_interaction", type="checkbox",
                               style="width:16px;height:16px;accent-color:#6366f1;margin:0;flex-shrink:0;"),
                    "Include interaction"
                  )
                )
              ),
            if (case == "two_vars") div()
          )
        ),

        # Hidden case tracker
        div(style="display:none;",
            selectInput("ind_case", label=NULL,
                        choices=c("two","multi","two_vars"),
                        selected=case)),

        # ── Results ──────────────────────────────────────────────────────
        uiOutput("indicator_results_ui")
      )
    )
  })

  # ── Tab observers ─────────────────────────────────────────────────────────
  observeEvent(input$ind_tab_two,      { updateSelectInput(session,"ind_case",selected="two") },      ignoreInit=TRUE)
  observeEvent(input$ind_tab_multi,    { updateSelectInput(session,"ind_case",selected="multi") },    ignoreInit=TRUE)
  observeEvent(input$ind_tab_two_vars, { updateSelectInput(session,"ind_case",selected="two_vars") }, ignoreInit=TRUE)

  # ── Results ───────────────────────────────────────────────────────────────
  output$indicator_results_ui <- renderUI({
    req(input$ind_y, input$ind_x)

    m_data <- tryCatch(ind_model(), error=function(e) NULL)
    if (is.null(m_data)) return(div(class="info-card warning", style="margin-top:20px;",
      HTML("<strong>⚠️ Model could not be built.</strong> Check your variable selections.")))

    m      <- m_data$model
    ms     <- summary(m)
    df     <- m_data$df
    y_var  <- m_data$y_var
    x_var  <- m_data$x_var
    cats   <- m_data$cat_vars
    refs   <- m_data$ref
    case   <- active_case()

    press  <- round(sum((residuals(m)/(1-hatvalues(m)))^2), 2)
    ovp    <- fmt_p(pf(ms$fstatistic[1],ms$fstatistic[2],ms$fstatistic[3],lower.tail=FALSE))

    tagList(
      # ── Key metrics strip ──────────────────────────────────────────
      div(
        style="display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:12px;margin-top:20px;margin-bottom:20px;",
        lapply(list(
          list("R²",        round(ms$r.squared,4),     "#6366f1"),
          list("Adj. R²",   round(ms$adj.r.squared,4), "#6366f1"),
          list("RSE",       round(ms$sigma,4),          "#1d4ed8"),
          list("F-stat",    round(ms$fstatistic[1],3),  "#1d4ed8"),
          list("p-value",   ovp,                        "#7c3aed"),
          list("PRESS",     press,                      "#7c3aed")
        ), function(x) {
          div(style=paste0("padding:14px 16px;border-radius:14px;background:#f9fafb;border:1px solid #e5e7eb;text-align:center;border-top:3px solid ",x[[3]],";"),
              div(style="font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.07em;color:#6b7280;margin-bottom:6px;", x[[1]]),
              div(style=paste0("font-size:20px;font-weight:800;color:",x[[3]],";"), x[[2]]))
        })
      ),

      # ── Coding scheme ──────────────────────────────────────────────
      div(class="content-card",
        div(class="card-header", "🏷️ Dummy Coding Scheme"),
        div(style="display:grid;gap:20px;",
          lapply(cats, function(d_var) {
            lvls   <- levels(df[[d_var]])
            ref    <- refs[d_var]
            dnames <- paste0(lvls[-1], " = 1")
            rows   <- lapply(lvls, function(lvl) {
              vals <- sapply(lvls[-1], function(o) if(lvl==o) 1L else 0L)
              c(list(Level=lvl, Reference=if(lvl==ref)"✓" else ""),
                setNames(as.list(vals), dnames))
            })
            df_out <- do.call(rbind, lapply(rows, function(r)
              as.data.frame(r, stringsAsFactors=FALSE, check.names=FALSE)))
            tagList(
              if(length(cats)>1) tags$strong(style="color:#132a23;", paste0("Variable: ", d_var)),
              div(class="table-card", style="margin-top:8px;",
                  DT::renderDataTable(
                    DT::datatable(df_out, rownames=FALSE,
                                  options=list(dom="t",scrollX=TRUE),
                                  class="cell-border stripe hover")
                  ))
            )
          })
        )
      ),

      # ── Coefficients ───────────────────────────────────────────────
      div(class="content-card",
        div(class="card-header", "📋 Coefficients & ANOVA"),
        div(style="display:grid;grid-template-columns:1fr;gap:24px;",
          div(div(style="font-weight:700;color:#374151;margin-bottom:10px;","Coefficients"),
              div(class="table-card", DTOutput("ind_coef_tbl"))),
          div(div(style="font-weight:700;color:#374151;margin-bottom:10px;","ANOVA"),
              div(class="table-card", DTOutput("ind_anova_tbl")))
        )
      ),

      # ── Partial F-test ─────────────────────────────────────────────
      div(class="content-card",
        div(class="card-header", "🔬 Partial F-Test"),
        uiOutput("ind_partial_f_ui")
      ),

      # ── Plot ───────────────────────────────────────────────────────
      div(class="content-card",
        div(class="card-header", "📈 Regression Plot"),
        withSpinner(plotOutput("ind_plot", height="460px"), color="#6366f1", type=4),
        div(class="download-section",
            downloadButton("download_ind_plot","📥 Download Plot", class="btn btn-info"))
      ),

      # ── Residuals ──────────────────────────────────────────────────
      div(class="content-card",
        div(class="card-header", "🔍 Residual Diagnostics"),
        fluidRow(
          column(6, withSpinner(plotOutput("ind_resid1",height="340px"), color="#6366f1", type=4)),
          column(6, withSpinner(plotOutput("ind_resid2",height="340px"), color="#6366f1", type=4))
        ),
        fluidRow(
          column(6, withSpinner(plotOutput("ind_resid3",height="340px"), color="#6366f1", type=4)),
          column(6, withSpinner(plotOutput("ind_resid4",height="340px"), color="#6366f1", type=4))
        )
      )
    )
  })

  # ── Table outputs ─────────────────────────────────────────────────────────
  output$ind_coef_tbl <- renderDT({
    m  <- ind_model()$model
    ct <- cbind(summary(m)$coefficients, confint(m))
    colnames(ct) <- c("Estimate","Std.Err","t","p","CI 2.5%","CI 97.5%")
    DT::datatable(round(ct,4), rownames=TRUE,
                  options=list(dom="t",scrollX=TRUE), class="cell-border stripe hover")
  })

  output$ind_anova_tbl <- renderDT({
    DT::datatable(anova(ind_model()$model), rownames=TRUE,
                  options=list(dom="t",scrollX=TRUE), class="cell-border stripe hover")
  })

  # ── Partial F-test ────────────────────────────────────────────────────────
  output$ind_partial_f_ui <- renderUI({
    m_data    <- ind_model()
    m_full    <- m_data$model
    m_reduced <- lm(as.formula(paste0("`",m_data$y_var,"` ~ `",m_data$x_var,"`")), data=m_data$df)
    ft     <- anova(m_reduced, m_full)
    f_stat <- ft$F[2]; f_p <- ft$`Pr(>F)`[2]
    df1    <- ft$Df[2]; df2 <- ft$Res.Df[2]
    rss_r  <- ft$RSS[1]; rss_f <- ft$RSS[2]
    is_sig <- !is.na(f_p) && f_p < 0.05

    div(
      style="display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px;",
      div(style="padding:16px;border-radius:12px;background:#f9fafb;border:1px solid #e5e7eb;",
          div(style="font-size:12px;font-weight:700;color:#6b7280;text-transform:uppercase;margin-bottom:8px;","Reduced (X only)"),
          div(style="font-size:12px;font-family:monospace;color:#132a23;margin-bottom:8px;",
              paste0(m_data$y_var," ~ ",m_data$x_var)),
          div(style="font-size:13px;",
              tags$b("RSS: "), round(rss_r,3), tags$br(),
              tags$b("R²: "),  round(summary(m_reduced)$r.squared,4))),
      div(style="padding:16px;border-radius:12px;background:#f9fafb;border:1px solid #e5e7eb;",
          div(style="font-size:12px;font-weight:700;color:#6b7280;text-transform:uppercase;margin-bottom:8px;","Full Model"),
          div(style="font-size:12px;font-family:monospace;color:#132a23;margin-bottom:8px;",
              paste0(m_data$y_var," ~ ",m_data$x_var," + ",paste(m_data$cat_vars,collapse=" + "))),
          div(style="font-size:13px;",
              tags$b("RSS: "), round(rss_f,3), tags$br(),
              tags$b("R²: "),  round(summary(m_full)$r.squared,4))),
      div(style="padding:16px;border-radius:12px;background:#f9fafb;border:1px solid #e5e7eb;",
          div(style="font-size:12px;font-weight:700;color:#6b7280;text-transform:uppercase;margin-bottom:8px;","F-Test"),
          div(style="font-size:13px;",
              tags$b("F: "),       if(is.na(f_stat)) "N/A" else round(f_stat,4), tags$br(),
              tags$b("df1: "),     df1, tags$br(),
              tags$b("df2: "),     df2, tags$br(),
              tags$b("p-value: "), fmt_p(f_p))),
      div(style=paste0("padding:16px;border-radius:12px;border:1px solid;font-size:13px;font-weight:600;",
                       if(is_sig) "background:#e7f7ef;border-color:#bfe9cf;color:#115737;"
                       else       "background:#eef2ff;border-color:#d4dcff;color:#283d8f;"),
          if(is.na(f_p)) "⚠️ Cannot compute."
          else if(is_sig)
            paste0("✅ p < 0.05 — '", paste(m_data$cat_vars,collapse="' & '"),
                   "' significantly improve the model.")
          else
            paste0("ℹ️ p ≥ 0.05 — The indicator variable(s) do not significantly improve the model."))
    )
  })

  # ── Regression plot ───────────────────────────────────────────────────────
  make_ind_plot <- function() {
    m_data <- ind_model()
    df <- m_data$df; m <- m_data$model
    y_var <- m_data$y_var; x_var <- m_data$x_var
    cats  <- m_data$cat_vars

    palette <- c("#6366f1","#1d4ed8","#a855f7","#ef4444","#f59e0b",
                 "#10b981","#ec4899","#a855f7","#84cc16","#f97316")

    if (length(cats) == 1) {
      d_var <- cats[1]
      lvls  <- levels(df[[d_var]])
      ref   <- m_data$ref[d_var]
      cols  <- palette[seq_len(length(lvls))]
      grp   <- as.integer(df[[d_var]])
      plot(df[[x_var]], df[[y_var]],
           col=adjustcolor(cols[grp], alpha.f=0.6), pch=19, cex=1.2,
           xlab=x_var, ylab=y_var,
           main=paste0(y_var," ~ ",x_var," + ",d_var),
           cex.main=1.2, cex.lab=1.1, col.main="#132a23", las=1, bty="l")
      grid(col="gray90", lty=1)
      x_seq <- seq(range(df[[x_var]])[1], range(df[[x_var]])[2], length.out=200)
      for (i in seq_along(lvls)) {
        nd <- data.frame(x_seq); names(nd) <- x_var
        nd[[d_var]] <- factor(lvls[i], levels=lvls)
        lines(x_seq, predict(m, newdata=nd), col=cols[i], lwd=3,
              lty=if(lvls[i]==ref) 1 else 2)
      }
      legend("topleft", legend=paste0(lvls, ifelse(lvls==ref," (ref)","")),
             col=cols, lwd=3, lty=ifelse(lvls==ref,1,2),
             pch=19, bty="n", cex=0.9, pt.cex=1.0)
    } else {
      # Two indicator variables: colour by first, shape by second
      d1 <- cats[1]; d2 <- cats[2]
      lvls1 <- levels(df[[d1]]); lvls2 <- levels(df[[d2]])
      cols   <- palette[seq_len(length(lvls1))]
      pchs   <- c(19,17,15,18,8,9,10,11,12,13)[seq_len(length(lvls2))]
      col_idx <- as.integer(df[[d1]])
      pch_idx <- as.integer(df[[d2]])
      plot(df[[x_var]], df[[y_var]],
           col=adjustcolor(cols[col_idx], alpha.f=0.6),
           pch=pchs[pch_idx], cex=1.3,
           xlab=x_var, ylab=y_var,
           main=paste0(y_var," ~ ",x_var," + ",d1," + ",d2),
           cex.main=1.2, cex.lab=1.1, col.main="#132a23", las=1, bty="l")
      grid(col="gray90", lty=1)
      x_seq <- seq(range(df[[x_var]])[1], range(df[[x_var]])[2], length.out=200)
      i <- 1
      for (l1 in lvls1) for (l2 in lvls2) {
        nd <- data.frame(x_seq); names(nd) <- x_var
        nd[[d1]] <- factor(l1, levels=lvls1)
        nd[[d2]] <- factor(l2, levels=lvls2)
        lines(x_seq, predict(m, newdata=nd),
              col=cols[which(lvls1==l1)], lwd=2,
              lty=which(lvls2==l2))
      }
      legend("topleft",
             legend=c(paste0(d1,": ",lvls1), paste0(d2,": ",lvls2)),
             col=c(cols, rep("gray40", length(lvls2))),
             lwd=2, lty=c(rep(1,length(lvls1)), seq_len(length(lvls2))),
             bty="n", cex=0.85)
    }
  }

  output$ind_plot <- renderPlot({ make_ind_plot() })

  output$download_ind_plot <- downloadHandler(
    filename=function() paste0("indicator_plot_",Sys.Date(),".png"),
    content =function(file){ png(file,width=1400,height=900,res=130); make_ind_plot(); dev.off() }
  )

  # ── Residual plots ────────────────────────────────────────────────────────
  rargs <- list(col=adjustcolor("#6366f1",alpha.f=0.6), pch=19, cex=1.0, col.main="#132a23")
  output$ind_resid1 <- renderPlot({ do.call(plot, c(list(ind_model()$model,which=1),rargs)) })
  output$ind_resid2 <- renderPlot({ do.call(plot, c(list(ind_model()$model,which=2),rargs)) })
  output$ind_resid3 <- renderPlot({ do.call(plot, c(list(ind_model()$model,which=3),rargs)) })
  output$ind_resid4 <- renderPlot({ do.call(plot, c(list(ind_model()$model,which=5),rargs)) })
}
