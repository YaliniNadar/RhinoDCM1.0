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
  ],
  DT[DTOutput, renderDT, datatable],
  data.table[fread],
)

box::use(
  app / view[ui_components, ],
  app / logic / storage,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    shinyjs::useShinyjs(),
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

    # Text output for displaying dimensions
    textOutput(ns("dataDimensions")),

    # File preview using DTOutput
    DTOutput(ns("filePreviewQ")),

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
    shinyjs::useShinyjs()

    # Conditional Rendering for Custom Separator
    output$custom_separator_input <- renderUI({
      if (input$separatorType == "") {
        textInput(session$ns("customSeparator"), "Enter Custom Separator:")
      }
    })

    # Dynamic rendering for the Next button based on file input
    output$nextButtonUI <- renderUI({
      ns <- session$ns  # Ensure we have the namespace function available

      if (!is.null(input$fileQ) && input$fileQ$size > 0) {
        actionButton(ns("nextButton"), "Next", class = "btn-primary")
      } else {
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

        # Exclude header rows if specified
        if (!is.null(input$headerColsRange) && input$headerColsRange != "") {
          # Check if the input is a single number or a range
          if (grepl("^\\d+$", input$headerColsRange)) {
            # If it's a single number
            col_to_exclude <- as.numeric(input$headerColsRange)
            print(col_to_exclude)
            data_temp <- data_temp[, -col_to_exclude, with = FALSE]
          } else if (grepl("^\\d+-\\d+$", input$headerColsRange)) {
            # If it's a range
            cols_to_exclude <- as.numeric(unlist(strsplit(input$headerColsRange, "-")))
            print(cols_to_exclude[1])
            print(cols_to_exclude[2])
            cols_to_exclude <- cols_to_exclude[1]:cols_to_exclude[2]
            print(cols_to_exclude)
            data_temp <- data_temp[, -cols_to_exclude, drop = FALSE]
          } else {
            # If it's neither a single number nor a range, handle the invalid input accordingly
            # For example, display a message to the user or take appropriate action
            # You may also choose to ignore the input in this case
          }
        }

        observeEvent(input$fileQ, {
          # Code to read and process the Q matrix file...
          num_cols_in_q_matrix <- ncol(data_temp)

          # Use data$numAttributes, which should be set by param_specs.R
          num_attributes <- data$numAttributes

          # Implementing the check
          if (!is.null(num_attributes) && num_cols_in_q_matrix != num_attributes) {
            # Providing feedback to the user
            shiny::showNotification("The number of attributes does not match the number of columns in the Q matrix. Please ensure they are equal.", type = "error")
            #disable button
            actionButton(session$ns("nextButton"), "Next", class = "btn-primary disabled", disabled = TRUE)
          } else {
            # Proceed with saving the Q matrix data to the application's state if the numbers match
            data$q_matrix <<- data_temp
          }
        })

        # Display file preview using DT
        output$filePreviewQ <- renderDT({
          datatable(data_temp)
        })

        # Save the modified data to q_matrix
        data$q_matrix <<- data_temp

        # Update text output to display dimensions
        output$dataDimensions <- renderText({
          paste("Dimensions: ", nrow(data_temp), " rows, ", ncol(data_temp), " columns")
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