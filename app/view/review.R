box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
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

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observe({
      tdcm$tdcm_test(data$q_matrix, data$ir_matrix)
    })

    ui_components$nb_server("nextButton", "/")
  })
}