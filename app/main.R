box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
  shiny.router[router_ui, router_server, route]
)

box::use(
  app/view[home, navbar, param_specs],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    div(
      navbar$ui(ns("navbar")),
    ),

    router_ui(
      route("/", home$home_ui(ns("home"))),
      route("param_specs", param_specs$param_specs_ui(ns("param_specs")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")
    home$home_server("home")
    param_specs$param_specs_server("param_specs")
  })
}