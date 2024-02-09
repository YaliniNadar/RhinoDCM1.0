box::use(
  shiny[NS, fluidPage, br, h2, h4, moduleServer, fluidRow, column, wellPanel, actionButton, textOutput, renderText]
)

box::use(
  app/view[ui_components]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Review"),

    # Main content blocks
    fluidRow(
      column(3, wellPanel(
        h4("Parameter Specs"),
        textOutput(ns("param_specs"))
      )),
      column(3, wellPanel(
        h4("Q-Matrix"),
        textOutput(ns("q_matrix"))
      )),
      column(3, wellPanel(
        h4("IR Matrix"),
        textOutput(ns("ir_matrix"))
      )),
      column(3, wellPanel(
        h4("Model Specs"),
        textOutput(ns("model_specs"))
      ))
    ),

    # Matrix previews
    fluidRow(
      column(6, wellPanel(
        h4("Q-Matrix Preview"),
        # Placeholder for Q-Matrix preview
      )),
      column(6, wellPanel(
        h4("IR Matrix Preview"),
        # Placeholder for IR Matrix preview
      ))
    ),

    # Navigation buttons
    fluidRow(
      column(6, ui_components$back_button(ns("backButton"))),
      column(6, ui_components$next_button(ns("nextButton")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define the text to be displayed in each section
    output$param_specs <- renderText({
      paste("Number of time points: 3",
            "Number of attributes measured: 10",
            "Attribute Names: a, b, c, d, e",
            "Is there a different Q-Matrix for each time point: No",
            "Number of items at a single time point: 3",
            sep = "\n")
    })

    output$q_matrix <- renderText("Q-Matrix content will be displayed here.")
    output$ir_matrix <- renderText("IR Matrix content will be displayed here.")
    output$model_specs <- renderText({
      paste("Invariance: Yes",
            "DCM to estimate: Different on each item",
            "Item 1: full DCM",
            "Item 2: TDCM2",
            "Item 3: TDCM1",
            sep = "\n")
    })
  })
}
