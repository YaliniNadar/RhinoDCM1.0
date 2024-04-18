box::use(
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
    observe,
    reactive,
    div,
    tags,
    plotOutput,
    renderPlot,
  ],
  shinyjs[useShinyjs, runjs],
  shinyStorePlus[initStore, setupStorage],
  shinyvalidate[InputValidator, sv_required, sv_optional, ],
  stringr[str_detect, str_trim, str_replace_all],
  rintrojs[introjsUI, introjs, introBox],
)

box::use(
  app/view[ui_components],
  app/logic[storage],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    initStore(),
    useShinyjs(),
    introjsUI(),

    # Trigger button for the tour
    introBox(
      actionButton(ns("startTour"), "Help", class = "btn-info"),
      data.step = 5,
      data.intro = "Click this button to start a guided tour of the application."
    ),
    h2("Parameter Specifications"),
    br(),
    introBox(
      tags$div(
        style = "display: flex; flex-direction: row; justify-content: space-between;",
        tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: #ffc400;"),
        tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
        tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
        tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;"),
        tags$div(style = "height: 10px; margin-bottom: 10px; width: 160px; background-color: white;")
      ),
      data.step = 5,
      data.intro = "These are three short lines next to each other."
    ),
    br(),
    # Input 1: Number of time points
    introBox(
      numericInput(ns("num_time_points"), "Enter number of time points: ", value = 1, min = 1),
      data.step = 1,
      data.intro = "Enter the total number of time points for your analysis."
    ),

    # Input 2: Number of attributes measured
    introBox(
      numericInput(ns("num_attributes"), "Enter number of attributes measured: ", value = 1, min = 1),
      data.step = 2,
      data.intro = "Enter the total number of attributes measured in your data."
    ),

    # Input 3: Attribute names separated by commas
    introBox(
      textInput(ns("attribute_names"), "Enter attribute names separated by commas (optional): "),
      data.step = 3,
      data.intro = "Enter the names of the attributes measured in your data, separated by commas."
    ),

    # Input 4: Q-Matrix for each time point
    introBox(
      radioButtons(ns("q_matrix_choice"), "Is there a different Q-Matrix for each time point?",
        choices = c("Yes", "No"), selected = "No"
      ),
      data.step = 4,
      data.intro = "Choose whether there is a different Q-Matrix for each time point in your data."
    ),
    uiOutput(ns("conditional_num_items")),
    # Input 5: Glowing dashes
    div(
      style = "display: flex; justify-content: flex-end;", # Aligns the buttons to the right
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI")) # Placeholder for dynamic Next button rendering
    ),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$startTour, {
      introjs(session)
    })

    observeEvent(input$num_attributes, {
      data$numAttributes <- input$num_attributes
    })

    observeEvent(input$num_items_single_time_point, {
      data$numTimeSinglePoint <- input$num_items_single_time_point
    })

    observeEvent(input$num_time_points, {
      data$numTimePoints <- input$num_time_points
    })

    # Initialize the input validator
    iv <- InputValidator$new()
    iv$add_rule(ns("num_time_points"), sv_required())
    iv$add_rule(ns("num_attributes"), sv_required())
    iv$add_rule(ns("attribute_names"), sv_optional())
    iv$add_rule(ns("q_matrix_choice"), sv_required())
    iv$enable()

    # Dynamic UI for conditional input based on Q-Matrix choice
    output$conditional_num_items <- renderUI({
      if (input$q_matrix_choice == "Yes") {
        textInput(ns("num_items_each_time_point"),
          "Enter number of items for each time point separated by commas: ",
          value = 1
        )
      } else {
        numericInput(ns("num_items_single_time_point"),
          "Enter number of items at a single time point: ",
          value = 1, min = 1
        )
      }
    })

    # Dynamic rendering for the Next button based on input validation
    output$nextButtonUI <- renderUI({
      # Use iv$is_valid() to check if all inputs meet the validation rules
      if (iv$is_valid()) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary")
      } else {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })

    # Save all input values to the data reactiveValues object
    observe({
      data$param_specs_data$num_time_points <- input$num_time_points
      data$param_specs_data$num_attributes <- input$num_attributes
      data$param_specs_data$attribute_names <- input$attribute_names
      data$param_specs_data$q_matrix_choice <- input$q_matrix_choice

      # Process attribute names into a list
      if (!is.null(input$attribute_names) && nchar(input$attribute_names) > 0) {
        # Split the attribute names by comma and remove leading/trailing whitespace
        attribute_names <- strsplit(input$attribute_names, ",")[[1]]
        attribute_names <- trimws(attribute_names)
        # Check if there are more than one attribute name
        if (length(attribute_names) > 1) {
          # Store attribute names as a list
          data$param_specs_data$attribute_names <- attribute_names
        } else {
          # If only one attribute name, store it as a character string
          data$param_specs_data$attribute_names <- attribute_names[1]
        }
      } else {
        # If input is empty, set attribute names to NULL or any default value you want
        data$param_specs_data$attribute_names <- NULL
      }

      # Save other input values based on conditions
      if (input$q_matrix_choice == "No") {
        data$param_specs_data$num_items <- input$num_items_single_time_point
      } else {
        # Check if input$num_items_each_time_point is not empty
        if (!is.null(input$num_items_each_time_point) &&
          nchar(input$num_items_each_time_point) > 0) {
          # Split the comma-separated values for each time point
          num_items_each_time_point <- as.numeric(unlist(strsplit(input$num_items_each_time_point, ","))) # nolint: line_length_linter.
          data$param_specs_data$num_items <- num_items_each_time_point
        } else {
          # Handle case when input$num_items_each_time_point is empty
          data$param_specs_data$num_items <- NULL # Or any default value you want to set
        }
      }
    })

    # Observe changes in inputs and update storage or perform other actions
    observe({
      db_name <- Sys.getenv("DB_NAME")
      prefix <- "app-param_specs-"
      fields <- c("num_time_points", "num_attributes", "attribute_names", "q_matrix_choice")
      # Assume storage$performIndexedDBRead is a function that reads data from IndexedDB
      storage$performIndexedDBRead(db_name, prefix, fields)
    })

    # Observe the Next button click event
    observeEvent(input$nextButton, {
      # Navigate to the q_matrix page
      shiny.router::change_page("q_matrix")
    })

    num_of_att_validation <- function(value) {
      # Extract individual attribute names
      attribute_names <- strsplit(value, ",")[[1]]
      num_attributes <- length(attribute_names)

      # Check if the number of attribute names matches the expected number
      if (any(trimws(attribute_names) == "")) {
        return("Attribute names cannot be just whitespace")
      } else if (num_attributes != input$num_attributes) {
        if (num_attributes < input$num_attributes) {
          return(paste(
            "Expected",
            input$num_attributes,
            "attribute names but only found",
            num_attributes
          ))
        } else {
          return(paste(
            "Expected",
            input$num_attributes,
            "attribute names but found",
            num_attributes
          ))
        }
      }
    }

    num_item_each_time_point_validation <- function(value) {
      # Extract individual numbers of items
      num_items_each_time_point <- as.numeric(unlist(strsplit(value, ",")))
      num_time_points <- input$num_time_points

      # Check if the number of items for each time point matches the expected number
      if (length(num_items_each_time_point) != num_time_points) {
        return(paste(
          "Expected",
          num_time_points,
          "numbers of items but found",
          length(num_items_each_time_point)
        ))
      }
    }

    # Print debugger for input values
    observe({
      print(paste("Number of time points:", input$num_time_points))
      print(paste("Number of attributes:", input$num_attributes))
      print(paste("Attribute names:", input$attribute_names))
      print(paste("Q-Matrix choice:", input$q_matrix_choice))
    })

    num_of_att_validation <- function(value) {
      attribute_names <- strsplit(value, ",")[[1]]
      num_attributes <- length(attribute_names)
      print(paste("Validating attribute names:", toString(attribute_names)))

      if (any(trimws(attribute_names) == "")) {
        return("Attribute names cannot be just whitespace")
      } else if (num_attributes != input$num_attributes) {
        print(paste("Mismatch: Expected", input$num_attributes, "found", num_attributes))
        if (num_attributes < input$num_attributes) {
          return(paste("Expected", input$num_attributes, "attribute names but only found", num_attributes))
        } else {
          return(paste("Expected", input$num_attributes, "attribute names but found", num_attributes))
        }
      }
    }

    num_item_each_time_point_validation <- function(value) {
      num_items_each_time_point <- as.numeric(unlist(strsplit(value, ",")))
      print(paste("Validating number of items for each time point:", toString(num_items_each_time_point)))

      if (length(num_items_each_time_point) != input$num_time_points) {
        print(paste("Mismatch: Expected", input$num_time_points, "found", length(num_items_each_time_point)))
        return(paste("Expected", input$num_time_points, "numbers of items but found", length(num_items_each_time_point)))
      }
    }

    iv <- InputValidator$new()
    iv$add_rule("num_time_points", sv_required())
    iv$add_rule("num_time_points", ~ if (!is.numeric(.)) "Input must be a number")
    iv$add_rule("num_time_points", ~ if (. != round(.)) "Input must be an integer")
    iv$add_rule("num_time_points", ~ if (. <= 0) "Input must be positive")
    iv$add_rule("num_attributes", sv_required())
    iv$add_rule("num_attributes", ~ if (!is.numeric(.)) "Input must be a number")
    iv$add_rule("num_attributes", ~ if (. != round(.)) "Input must be an integer")
    iv$add_rule("num_attributes", ~ if (. <= 0) "Input must be positive")
    iv$add_rule("attribute_names", sv_optional()) # check this
    iv$add_rule("attribute_names", ~ if (any(grepl(" ", trimws(strsplit(., ",")[[1]])))) {
      "Attribute names cannot contain whitespace"
    })
    iv$add_rule("attribute_names", num_of_att_validation)
    iv$add_rule("q_matrix_choice", sv_required())

    q_choice_yes <- InputValidator$new()
    q_choice_yes$condition(~ input$q_matrix_choice == "No")
    q_choice_yes$add_rule("num_items_single_time_point", sv_required())
    q_choice_yes$add_rule("num_items_single_time_point", ~ if (!is.numeric(.)) "Input must be a number")
    q_choice_yes$add_rule("num_items_single_time_point", ~ if (. != round(.)) "Input must be an integer")
    q_choice_yes$add_rule("num_items_single_time_point", ~ if (. <= 0) "Input must be positive")

    q_choice_no <- InputValidator$new()
    q_choice_no$condition(~ input$q_matrix_choice == "Yes")
    q_choice_no$add_rule("num_items_each_time_point", sv_required())
    q_choice_no$add_rule("num_items_each_time_point", ~ if (any(grepl("[^0-9,]", .))) {
      "Input must be a comma-separated list of numbers"
    })
    q_choice_no$add_rule("num_items_each_time_point", num_item_each_time_point_validation)
    q_choice_no$add_rule("num_items_each_time_point", ~ if (any(grepl(" ", trimws(strsplit(., ",")[[1]])))) {
      "Attribute names cannot contain whitespace"
    })

    iv$add_validator(q_choice_yes)
    iv$add_validator(q_choice_no)

    iv$enable()
    ui_components$nb_server("nextButton", "q_matrix")
  })
}
