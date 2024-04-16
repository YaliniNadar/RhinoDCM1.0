box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    tags,
    fileInput,
    radioButtons,
    textInput,
    checkboxInput,
    moduleServer,
    observe,
    observeEvent,
    renderUI,
    uiOutput,
    actionButton,
    div,
    textOutput,
    renderText,
    tagList,
    HTML,
  ],
  shinyjs[runjs],
  DT[DTOutput, renderDT, datatable, JS],
  data.table[fread],
)

box::use(
  app/view[ui_components, ],
  app/logic/storage,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload Q-Matrix File"),
    br(),


    # Input: Upload Q-Matrix file
    fileInput(ns("fileQ"), "Choose Q-Matrix File"),

    # Input: Separator type
    radioButtons(ns("separatorType"), "Separator Type:",
      choices = c("Tab" = "\t", "Comma" = ",", "Space" = " ", "Custom" = ""),
      selected = ","
    ),

    # Conditional Separator
    uiOutput(ns("custom_separator_input")),

    # Input: Additional options
    checkboxInput(ns("excludeHeaders"), "First Row Contains Column Names", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "First Column Contains Row IDs", value = FALSE),
    uiOutput(ns("errorBox")),

    # Text output for displaying dimensions
    textOutput(ns("dataDimensions")),

    # File preview using DTOutput
    DTOutput(ns("filePreviewQ")),
    div(
      style = "display: flex; justify-content: flex-end;", # Aligns the buttons to the right
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI")) # Placeholder for dynamic Next button rendering
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    shinyjs::useShinyjs()

    observeEvent(input$num_rows_in_q_matrix, {
      num_rows_in_q_matrix <- input$num_rows_in_q_matrix
    })
    # Conditional Rendering for Custom Separator
    output$custom_separator_input <- renderUI({
      if (input$separatorType == "") {
        tagList(
          textInput(session$ns("customSeparator"), "Enter Custom Separator:"),
          tags$script(HTML(sprintf("$(document).on('shiny:inputchanged', function(event) {
            if (event.name === '%s') {
              $('#%s').attr('maxlength', 1);
            }
          });", session$ns("customSeparator"), session$ns("customSeparator"))))
        )
      }
    })

    # Dynamic rendering for the Next button based on file input
    output$nextButtonUI <- renderUI({
      ns <- session$ns # Ensure we have the namespace function available

      if (is.null(input$fileQ)) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })

    # Observe the Next button click event
    observeEvent(input$nextButton, {
      # Navigate to the q_matrix page
      shiny.router::change_page("ir_matrix")
    })

    observe({
      # Read the uploaded file
      file <- input$fileQ
      if (!is.null(file$datapath)) {
        separator <- input$separatorType
        if (separator == "") {
          data_temp <- fread(file$datapath,
            sep = input$customSeparator,
            header = input$excludeHeaders,
            check.names = FALSE,
            quote = "",
          )
        } else {
          data_temp <- fread(file$datapath,
            sep = input$separatorType,
            header = input$excludeHeaders,
            quote = "",
          )
        }

        # Exclude ID columns if specified
        if (input$excludeIdColumns) {
          # Define which columns to exclude (e.g., first column)
          id_columns <- 1
          # Remove ID columns from the DataTable
          data_temp <- data_temp[, -id_columns, with = FALSE]
        }

        observeEvent(input$fileQ, {
          # Code to read and process the Q matrix file
          num_cols_in_q_matrix <- ncol(data_temp)
          num_rows_in_q_matrix <- nrow(data_temp)
          # Use data$numAttributes, which should be set by param_specs.R
          num_attributes <- data$numAttributes
          num_items_for_single_time <- data$numTimeSinglePoint
          data$num_rows_in_q_matrix <- nrow(data_temp)

          error_message <- NULL

          if (!is.null(num_items_for_single_time) && num_rows_in_q_matrix !=
            num_items_for_single_time) {
            error_message <- paste("The number of rows in the Q matrix (", num_rows_in_q_matrix,
              ") does not match the number of items (",
              num_items_for_single_time, "). Please ensure they are equal.",
              sep = ""
            )
          } else if (!is.null(num_attributes) && num_cols_in_q_matrix != num_attributes) {
            error_message <- paste("The number of attributes (", num_attributes,
                                   ") does not match the number of columns (",
                                   num_cols_in_q_matrix, ") in the Q matrix.
                                   Please ensure they are equal.", sep = "")
          }

          if (!is.null(error_message)) {
            output$errorBox <- renderUI({
              div(
                class = "alert alert-danger", role = "alert",
                shiny::tags$strong("Error: "), error_message
              )
            })
            output$nextButtonUI <- renderUI({
              actionButton(session$ns("nextButton"), "Next",
                class = "btn-primary disabled",
                disabled = TRUE
              )
            })
          } else {
            # Clear the error box if there are no errors
            output$errorBox <- renderUI({
              NULL
            })

            output$nextButtonUI <- renderUI({
              actionButton(session$ns("nextButton"), "Next", class = "btn-primary")
            })
            # Proceed with saving the Q matrix data to the application's state if the numbers match
            data$q_matrix <<- data_temp
          }
        })


        # Display file preview using DT
        output$filePreviewQ <- renderDT({
          datatable(data_temp,
            options = list(
              searching = FALSE,
              initComplete = JS(ui_components$format_pagination())
            )
          )
        })

        # Save the modified data to q_matrix
        data$q_matrix <<- data_temp

        # Update text output to display dimensions
        output$dataDimensions <- renderText({
          paste("Dimensions: ", nrow(data_temp), " rows, ", ncol(data_temp), " columns")
          num_cols_in_q_matrix <- ncol(data_temp)
        })
      } else {
        # Clear the preview if no file is selected
        output$filePreviewQ <- renderDT(NULL)
      }
    })
    observe({
      db_name <- Sys.getenv("DB_NAME")
      prefix <- "app-q_matrix-"
      fields <- c("separatorType", "excludeHeaders", "excludeIdColumns", "fileQ")
      storage$performIndexedDBRead(db_name, prefix, fields)
    })

    ui_components$nb_server("nextButton", "ir_matrix")
  })
}
