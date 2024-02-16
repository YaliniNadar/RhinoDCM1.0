box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    actionButton,
    observeEvent,
    moduleServer,
    observe,
    renderText,
    textOutput,
  ]
)

box::use(
  app/view[ui_components, ],
  app/logic/tdcm,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Review"),
    br(),

    actionButton(ns("testButton"), "Click Me"),

    textOutput(ns("resultOutput")),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$testButton, {
      result <- tdcm$tdcm_test(data$q_matrix, data$ir_matrix)
      output$resultOutput <- renderText(result)
    })

    ui_components$nb_server("nextButton", "/")
  })
}