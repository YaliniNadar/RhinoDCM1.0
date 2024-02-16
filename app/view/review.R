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

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$testButton, {
      tdcm$tdcm_test(data$q_matrix, data$ir_matrix)
    })

    ui_components$nb_server("nextButton", "/")
  })
}