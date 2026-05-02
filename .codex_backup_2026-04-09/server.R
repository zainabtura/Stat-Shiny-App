app_server <- function(input, output, session) {
  state <- create_app_state(input, output, session)

  register_core_server(input, output, session, state)
  register_dashboard_server(input, output, session, state)
  register_preview_server(input, output, session, state)
  register_analysis_server(input, output, session, state)
  register_regression_server(input, output, session, state)
}
