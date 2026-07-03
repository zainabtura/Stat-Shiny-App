app_server <- function(input, output, session) {
  state <- create_app_state(input, output, session)

  register_core_server(input, output, session, state)
  register_dashboard_server(input, output, session, state)
  register_descriptive_server(input, output, session, state)
  register_analysis_server(input, output, session, state)
  register_regression_server(input, output, session, state)
  register_indicator_server(input, output, session, state)
  register_influence_server(input, output, session, state)
  register_polynomial_server(input, output, session, state)
  register_spline_server(input, output, session, state)
  register_glm_server(input, output, session, state)
}
