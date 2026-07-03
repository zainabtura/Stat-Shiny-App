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
          div(class = "card-header", "\U0001F4C8 Numeric Variables Quick Stats"),
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
          div(class = "card-header", "\U0001F3F7️ Categorical Variables Distribution"),
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
          div(class = "card-header", "\U0001F517 Top Correlations"),
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
