box::use(
<<<<<<< HEAD
  shiny[moduleServer,
        NS,
        fluidPage,
        h2,
        p,
        br,
        HTML,
        numericInput,
        textInput,
        radioButtons,
        actionButton,
        observeEvent,
        uiOutput,
        renderUI,
        observe,
        reactive,
        div],
=======
  shiny[
    moduleServer,
    NS,
    fluidPage,
    h2,
    p,
    br,
    HTML,
    numericInput,
    textInput,
    radioButtons,
    actionButton,
    observeEvent,
    uiOutput,
    renderUI,
    observe
  ],
>>>>>>> c6960b2616ffec0e89dfeb7886bc777dfa471c35
  shinyjs[useShinyjs, runjs],
  shinyStorePlus[initStore, setupStorage],
  shinyvalidate[InputValidator, sv_required],
  stringr[str_detect, str_trim, str_replace_all],
)

box::use(
  app / view[ui_components],
  app / logic / storage,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    initStore(),
    useShinyjs(),
    h2("Parameter Specifications"),
    br(),

    # Input 1: Number of time points
    numericInput(ns("num_time_points"), "Enter number of time points: ", value = 1, min = 1),

    # Input 2: Number of attributes measured
    numericInput(ns("num_attributes"), "Enter number of attributes measured: ", value = 1, min = 1),

    # Input 3: Attribute names separated by commas
    textInput(ns("attribute_names"), "Enter attribute names separated by commas: "),

    # Input 4: Q-Matrix for each time point
    radioButtons(ns("q_matrix_choice"), "Is there a different Q-Matrix for each time point?",
                 choices = c("Yes", "No"), selected = NULL),

    uiOutput(ns("conditional_num_items")),

<<<<<<< HEAD
    div(
      style = "display: flex; justify-content: flex-end;",  # Aligns the buttons to the right
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI"))  # Placeholder for dynamic Next button rendering
    )
=======
    # ui_components$next_button(ns("nextButton")),
    # ui_components$back_button(ns("backButton")),
>>>>>>> c6960b2616ffec0e89dfeb7886bc777dfa471c35
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize the input validator
    iv <- InputValidator$new()
    iv$add_rule(ns("num_time_points"), sv_required())
    iv$add_rule(ns("num_attributes"), sv_required())
    iv$add_rule(ns("attribute_names"), sv_required())
    iv$add_rule(ns("q_matrix_choice"), sv_required())
    iv$enable()

    # Dynamic UI for conditional input based on Q-Matrix choice
    output$conditional_num_items <- renderUI({
      if (input$q_matrix_choice == "Yes") {
        textInput(ns("num_items_each_time_point"), "Enter number of items for each time point separated by commas (no spaces): ")
      } else {
        numericInput(ns("num_items_single_time_point"), "Enter number of items at a single time point: ", value = 1, min = 1)
      }
    })

<<<<<<< HEAD
    # Dynamic rendering for the Next button based on input validation
    output$nextButtonUI <- renderUI({
      # Use iv$is_valid() to check if all inputs meet the validation rules
      if (iv$is_valid()) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary")
      } else {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })

    # Observe changes in inputs and update storage or perform other actions
=======
    # Save all input values to the data reactiveValues object
    observe({
      data$param_specs_data$num_time_points <- input$num_time_points
      data$param_specs_data$num_attributes <- input$num_attributes
      data$param_specs_data$attribute_names <- input$attribute_names
      data$param_specs_data$q_matrix_choice <- input$q_matrix_choice

      # Save other input values based on conditions
      if (input$q_matrix_choice == "No") {
        data$param_specs_data$num_items <- input$num_items_single_time_point
      } else {
        # Check if input$num_items_each_time_point is not empty
        if (!is.null(input$num_items_each_time_point) &&
          nchar(input$num_items_each_time_point) > 0) {
          # Split the comma-separated values for each time point
          data$param_specs_data$num_items_each_time_point <- input$num_items_each_time_point
          # num_items_each_time_point <- as.numeric(unlist(strsplit(input$num_items_each_time_point, ","))) # nolint: line_length_linter.
          # data$param_specs_data$num_items_each_time_point <- num_items_each_time_point
        } else {
          # Handle case when input$num_items_each_time_point is empty
          data$param_specs_data$num_items <- NULL # Or any default value you want to set
        }
      }
    })

    # Observing Storage
>>>>>>> c6960b2616ffec0e89dfeb7886bc777dfa471c35
    observe({
      db_name <- Sys.getenv("DB_NAME")
      prefix <- 'app-param_specs-'
      fields <- c('num_time_points', 'num_attributes', 'attribute_names', 'q_matrix_choice')
      # Assume storage$performIndexedDBRead is a function that reads data from IndexedDB
      storage$performIndexedDBRead(db_name, prefix, fields)
    })

    # Observe the Next button click event
    observeEvent(input$nextButton, {
      # Navigate to the q_matrix page
      shiny.router::change_page("q_matrix")
    })

    iv <- InputValidator$new()
    iv$add_rule("num_time_points", sv_required())
    iv$add_rule("num_time_points", ~ if (!is.numeric(.)) "Input must be a number")
    iv$add_rule("num_time_points", ~ if (. != round(.)) "Input must be an integer")
    iv$add_rule("num_time_points", ~ if (. <= 0) "Input must be positive")
    iv$add_rule("num_attributes", sv_required())
    iv$add_rule("num_attributes", ~ if (!is.numeric(.)) "Input must be a number")
    iv$add_rule("num_attributes", ~ if (. != round(.)) "Input must be an integer")
    iv$add_rule("num_attributes", ~ if (. <= 0) "Input must be positive")
    iv$add_rule("attribute_names", sv_required()) # check this
    iv$add_rule("attribute_names", ~ if (any(grepl(" ", trimws(strsplit(., ",")[[1]])))) {
      "Every space must be preceded by a comma"
    })
    iv$add_rule("q_matrix_choice", sv_required())
    iv$enable()

    ui_components$nb_server("nextButton", "q_matrix")

  })
}
