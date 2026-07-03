create_app_state <- function(input, output, session) {
  upload_time <- reactiveVal(NULL)
  current_tab <- reactiveVal("tab_home")
  built_regression <- reactiveVal(NULL)
  selected_sample_dataset <- reactiveVal(NULL)

  sample_dataset_dir <- "datasets"
  sample_dataset_files <- if (dir.exists(sample_dataset_dir)) {
    list.files(
      sample_dataset_dir,
      pattern = "\\.(csv|xlsx|xls)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character(0)
  }

  sample_dataset_choices <- setNames(
    sample_dataset_files,
    tools::file_path_sans_ext(basename(sample_dataset_files))
  )

  data_source_info <- reactive({
    if (!is.null(input$file)) {
      return(list(
        kind = "upload",
        name = input$file$name,
        path = input$file$datapath,
        ext = tolower(tools::file_ext(input$file$name))
      ))
    }

    sample_path <- selected_sample_dataset()
    if (!is.null(sample_path) && file.exists(sample_path)) {
      return(list(
        kind = "sample",
        name = basename(sample_path),
        path = sample_path,
        ext = tolower(tools::file_ext(sample_path))
      ))
    }

    NULL
  })

  data_in <- reactive({
    source_info <- data_source_info()
    req(source_info)

    ext <- source_info$ext

    df <- NULL
    if (ext == "csv") {
      df <- readr::read_csv(source_info$path, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(source_info$path)
    } else {
      validate("Unsupported file type")
    }

    if (isTRUE(input$stringsAsFactors)) {
      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
      }
    }

    df
  })

  observeEvent(data_source_info(), {
    req(data_source_info())
    upload_time(Sys.time())
  }, ignoreInit = TRUE)

  numeric_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is.numeric)]
  })

  categorical_vars <- reactive({
    df <- data_in()
    names(df)[sapply(df, is_categorical)]
  })

  list(
    upload_time = upload_time,
    current_tab = current_tab,
    built_regression = built_regression,
    data_source_info = data_source_info,
    data_in = data_in,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    sample_dataset_choices = sample_dataset_choices,
    selected_sample_dataset = selected_sample_dataset
  )
}
