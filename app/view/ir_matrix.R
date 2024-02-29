box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    fileInput,
    radioButtons,
    textInput,
    checkboxInput,
    conditionalPanel,
    moduleServer,
    observe,
    renderUI,
    uiOutput,
    div,
    actionButton,
    observeEvent
  ],
  DT[DTOutput, renderDT, datatable],
  data.table[fread],
)

box::use(
  app / view[ui_components, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload IR-Matrix File"),
    br(),

    # Input: Upload IR-Matrix file
    fileInput(ns("fileIR"), "Choose IR-Matrix File"),

    # Input: Separator type
    radioButtons(ns("separatorType"), "Separator Type:",
      choices = c("Tab" = "\t", "Comma" = ",", "Custom" = ""),
      selected = ","
    ),

    # Conditional Separator
    uiOutput(ns("custom_separator_input")),

    # Input: Additional options
    checkboxInput(ns("excludeHeaders"), "Exclude Header Row", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "Exclude ID Columns", value = FALSE),

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
server <- function(id) {
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
          data <- fread(file$datapath,
            sep = input$customSeparator,
            header = !input$excludeHeaders,
            check.names = FALSE
          )
        } else {
          data <- fread(file$datapath, sep = input$separatorType, header = !input$excludeHeaders)
        }
        # Display file preview using DT
        output$filePreviewIR <- renderDT({
          datatable(data, editable = TRUE)
        })
      } else {
        # Clear the preview if no file is selected
        output$filePreviewIR <- renderDT(NULL)
      }
    })

    ui_components$nb_server("nextButton", "model_specs")
  })
}
