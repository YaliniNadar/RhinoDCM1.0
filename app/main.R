box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
  shiny.router[router_ui, router_server, route]
)

box::use(
  app/view[home, ui_components, param_specs, q_matrix, ir_matrix],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    div(
      ui_components$navbar_ui(ns("navbar")),
    ),

    router_ui(
      route("/", home$ui(ns("home"))),
      route("param_specs", param_specs$ui(ns("param_specs"))),
      route("q_matrix", q_matrix$ui(ns("q_matrix"))),
      route("ir_matrix", ir_matrix$ui(ns("ir_matrix"))),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")
    home$server("home")
    param_specs$server("param_specs")
    q_matrix$server("q_matrix")
    ir_matrix$server("ir_matrix")
  })
}
