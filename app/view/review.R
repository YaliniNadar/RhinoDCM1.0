box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    h4,
    moduleServer,
    column,
    wellPanel,
    actionButton,
    textOutput,
    renderText,
    fluidRow,
    observeEvent,
    observe,
    renderUI,
    tagList,
    uiOutput,
    HTML,
  ],
  DT[
    datatable,
  ],
  utils[
    head,
  ]
)

box::use(
  app / view[ui_components],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Review"),
    br(),
    br(),
    # Main content blocks
    fluidRow(
      column(6, wellPanel(
        h4("Parameter Specs"),
        uiOutput(ns("param_specs_ui"))
      )),
      column(6, wellPanel(
        h4("Model Specs"),
        textOutput(ns("model_specs"))
      ))
    ),

    # Matrix previews
    fluidRow(
      column(6, wellPanel(
        uiOutput(ns("q_matrix")),
        style = "max-width: 100%; overflow-x: auto;"
      )),
      column(6, wellPanel(
        uiOutput(ns("ir_matrix")),
        style = "max-width: 100%; overflow-x: auto;"
      ))
    ),

    # Navigation buttons
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton"))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Save all input values to the data reactiveValues object
    observe({
      file_header_list <- colnames(data$q_matrix)
      file_header_list <- gsub("\\\"", "", file_header_list)

      # Check if data$param_specs$attribute_names is not null
      if (!is.null(data$param_specs_data$attribute_names)) {
        data$review$col_names <- data$param_specs_data$attribute_names
      } else if (!is.null(file_header_list)) {
        # Use header_list
        data$review$col_names <- file_header_list
      }

    })


    generate_model_specs <- function() {
      item_param <- data$model_specs_data$itemParameter
      dcm_estimate <- data$model_specs_data$dcmEstimate

      # Initialize model_specs as an empty string
      model_specs <- ""

      # Append item parameter to model_specs
      model_specs <-
        paste(model_specs,
              paste("Item Parameter Assumed:", ifelse(is.null(item_param), "N/A", item_param)))

      if (!is.null(dcm_estimate)) {
        if (is.character(dcm_estimate)) {
          # Append DCM to estimate if it's character
          model_specs <- paste(model_specs, paste("DCM to estimate:", dcm_estimate))
        }
      }

      # Collapse model_specs into a single string with newline characters
      return(model_specs)
    }


    # Define the text to be displayed in each section
    output$param_specs_ui <- renderUI({
      # Function to generate the text for parameter specifications
      generate_param_specs <- function() {
        num_time_points <- data$param_specs_data$num_time_points
        num_attributes <- data$param_specs_data$num_attributes
        attribute_names <- data$review$col_names
        q_matrix_choice <- data$param_specs_data$q_matrix_choice
        num_items_single_time_point <- data$param_specs_data$num_items
        num_items_each_time_point <- data$param_specs_data$num_items_each_time_point

        if (is.null(q_matrix_choice) || length(q_matrix_choice) == 0) {
          q_matrix_choice_message <- "N/A"
        } else {
          q_matrix_choice_message <- q_matrix_choice
        }

        param_specs <- paste(
          "Number of time points:", ifelse(is.null(num_time_points), "N/A", num_time_points),
          "Number of attributes measured:", ifelse(is.null(num_attributes), "N/A", num_attributes),
          "Attribute Names:",
          ifelse(is.null(attribute_names), "N/A", paste(attribute_names, collapse = ", ")),
          "Is there a different Q-Matrix for each time point:",
          ifelse(is.null(q_matrix_choice), "N/A", q_matrix_choice),
          if (q_matrix_choice_message == "No") {
            paste("Number of items at a single time point:",
                  ifelse(is.null(num_items_single_time_point), "N/A", num_items_single_time_point))
          } else if (q_matrix_choice_message == "Yes") {
            paste("Number of items at each time point:",
                  ifelse(is.null(num_items_each_time_point), "N/A", num_items_each_time_point))
          },
          sep = "<br>"
        )

        return(param_specs)
      }

      # Call the function to generate 'param_specs'
      param_specs_text <- generate_param_specs()

      # Use the generated 'param_specs_text' with HTML()
      HTML(param_specs_text)
    })

    output$q_matrix <- renderUI({
      if (!is.null(data$q_matrix)) {
        q_matrix_data <- data$q_matrix
        num_rows <- nrow(q_matrix_data)
        num_cols <- ncol(q_matrix_data)

        if (num_rows >= 5) {
          q_matrix_head <- head(q_matrix_data, 5)
        } else {
          q_matrix_head <- q_matrix_data
        }

        # Convert the first 5 rows of the Q-Matrix data frame to an HTML table
        q_matrix_html <-
          datatable(q_matrix_head,
            options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
          )

        if (num_rows >= 5) {
          tagList(
            h4("Q-Matrix Preview (First 5 Rows)"),
            h4(paste("Dimensions of Entire Matrix:", num_rows, "x", num_cols)),
            q_matrix_html
          )
        } else if (num_rows < 5) {
          tagList(
            h4("Q-Matrix Preview"),
            h4(paste("Dimensions of Entire Matrix:", num_rows, "x", num_cols)),
            q_matrix_html
          )
        } else {
          h4("Q-Matrix Preview is not available.")
        }
      }
    })

    output$ir_matrix <- renderUI({
      if (!is.null(data$ir_matrix)) {
        ir_matrix_data <- data$ir_matrix
        num_rows <- nrow(ir_matrix_data)
        num_cols <- ncol(ir_matrix_data)

        if (num_rows >= 5) {
          ir_matrix_head <- head(ir_matrix_data, 5)
        } else {
          ir_matrix_head <- ir_matrix_data
        }

        # Convert the first 5 rows of the Q-Matrix data frame to an HTML table
        ir_matrix_html <-
          datatable(ir_matrix_head,
            options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
          )

        if (num_rows >= 5) {
          tagList(
            h4("IR-Matrix Preview (First 5 Rows)"),
            h4(paste("Dimensions of Entire Matrix:", num_rows, "x", num_cols)),
            ir_matrix_html
          )
        } else if (num_rows < 5) {
          tagList(
            h4("IR-Matrix Preview"),
            h4(paste("Dimensions of Entire Matrix:", num_rows, "x", num_cols)),
            ir_matrix_html
          )
        } else {
          h4("IR-Matrix Preview is not available.")
        }
      }
    })

    output$model_specs <- renderText({
      generate_model_specs()
    })

    ui_components$nb_server("nextButton", "tdcm_test")
  })
}
