box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    moduleServer,
  ]
)

box::use(
  app/view[ui_components, ],
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ui_components$nb_server("nextButton", "/")
  })
}