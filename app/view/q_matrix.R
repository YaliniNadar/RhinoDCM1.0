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
        moduleServer,
        observe,
        renderUI,
        uiOutput],
  DT[DTOutput, renderDT, datatable],
  data.table[fread],
)

box::use(
  app/view[ui_components, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload Q-Matrix File"),
    br(),

    # Input: Upload Q-Matrix file
    fileInput(ns("fileQ"), "Choose Q-Matrix File"),

    # File preview using DTOutput
    DTOutput(ns("filePreviewQ")),

    # Input: Separator type
    radioButtons(ns("separatorType"), "Separator Type:",
                 choices = c("Tab" = "\t", "Comma" = ",", "Custom" = ""),
                 selected = ","),

    # Conditional Separator
    uiOutput(ns("custom_separator_input")),

    # Input: Additional options
    checkboxInput(ns("excludeHeaders"), "Exclude Header Rows", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "Exclude ID Columns", value = FALSE),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
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

    observe({
      # Read the uploaded file
      file <- input$fileQ
      if (!is.null(file$datapath)) {
        separator <- input$separatorType
        if (separator == "") {
          data <- fread(file$datapath,
                        sep = input$customSeparator,
                        header = !input$excludeHeaders,
                        check.names = FALSE)
        } else {
          data <- fread(file$datapath, sep = input$separatorType, header = !input$excludeHeaders)
        }
        # Display file preview using DT
        output$filePreviewQ <- renderDT({
          datatable(data, editable = TRUE)
        })
      } else {
        # Clear the preview if no file is selected
        output$filePreviewQ <- renderDT(NULL)
      }
    })

    ui_components$nb_server("nextButton", "ir_matrix")
  })
}