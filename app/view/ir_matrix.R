box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    fileInput,
    renderText,
    textOutput,
    radioButtons,
    textInput,
    checkboxInput,
    conditionalPanel,
    moduleServer,
    observe,
    observeEvent,
    renderUI,
    uiOutput,
    div,
    actionButton,
  ],
  DT[DTOutput, renderDT, datatable],
  data.table[fread],
)

box::use(
  app / view[ui_components, ],
  app/logic/storage,
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  fluidPage(
    h2("Upload IR-Matrix File"),
    br(),

    # Input: Upload IR-Matrix file
    fileInput(ns("fileIR"), "Choose IR-Matrix File"),

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
    DTOutput(ns("filePreviewIR")),

    div(
      style = "display: flex; justify-content: flex-end;",  # Aligns the buttons to the right
      ui_components$back_button(ns("backButton")),
      uiOutput(ns("nextButtonUI"))  # Placeholder for dynamic Next button rendering
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Conditional Rendering for Custom Separator
    output$custom_separator_input <- renderUI({
      if (input$separatorType == "") {
        textInput(session$ns("customSeparator"), "Enter Custom Separator:")
      }
    })

    # Dynamic rendering for the Next button based on file input
    output$nextButtonUI <- renderUI({
      ns <- session$ns  # Ensure we have the namespace function available

      # Check if the file input is not NULL and has a size greater than 0
      if (!is.null(input$fileIR) && input$fileIR$size > 0) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary")
      } else {
        actionButton(ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
      }
    })


    # Observe the Next button click event
    observeEvent(input$nextButton, {
      # Navigate to the q_matrix page
      shiny.router::change_page("model_specs")
    })

    observe({
      # Read the uploaded file
      file <- input$fileIR
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

        observeEvent(input$fileIR, {
          # Read IR matrix columns
          num_cols_in_ir_matrix <- ncol(data_temp)
          # Code to read Q rows and time points
          num_rows_in_q_matrix <- data$num_rows_in_q_matrix
          num_time_points <- data$numTimePoints #check

          error_message <- NULL

          num_cols_in_ir_matrix <- ncol(data_temp)
          print(paste("num_cols_in_ir_matrix: ", num_cols_in_ir_matrix))

          num_rows_in_q_matrix <- data$num_rows_in_q_matrix
          print(paste("num_rows_in_q_matrix: ", num_rows_in_q_matrix))

          num_time_points <- data$numTimePoints
          print(paste("num_time_points: ", num_time_points))

          if (!is.null(num_cols_in_ir_matrix) && !is.null(num_rows_in_q_matrix) && !is.null(num_time_points) && num_rows_in_q_matrix * num_time_points != num_cols_in_ir_matrix) {
            error_message <- paste("The number of IR columns does not equal the number of columns in the
            Q matrix multiplied by time points.", sep = "")
          }

          if (!is.null(error_message)) {
            output$errorBox <- renderUI({
              div(class = "alert alert-danger", role = "alert",
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
            output$errorBox <- renderUI({ NULL })

            output$nextButtonUI <- renderUI({
              actionButton(session$ns("nextButton"), "Next", class = "btn-primary")
            })
            # Proceed with saving the IR matrix data to the application's state if the numbers match
            data$ir_matrix <<- data_temp
          }
        })

        # Display file preview using DT
        output$filePreviewIR <- renderDT({
          datatable(data_temp,
                    options = list(
                      autoWidth = TRUE,
                      scrollX = TRUE
                    ), )
        })

        # Save the modified data to ir_matrix
        data$ir_matrix <<- data_temp

        # Update text output to display dimensions
        output$dataDimensions <- renderText({
          paste("Dimensions: ", nrow(data_temp), " rows, ", ncol(data_temp), " columns")
        })
      } else {
        # Clear the data if the file is NULL
        output$filePreviewIR <- renderDT(NULL)
      }
    })

    ui_components$nb_server("nextButton", "model_specs")
  })
}
