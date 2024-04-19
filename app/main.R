box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, reactiveValues],
  shiny.router[router_ui, router_server, route]
)

box::use(
  app / view[
    home, ui_components, param_specs, q_matrix, ir_matrix, model_specs, review, tdcm_test,
    primary_aggregate_results, primary_individual_results, secondary_results, table_helper
  ],
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
      route("model_specs", model_specs$ui(ns("model_specs"))),
      route("review", review$ui(ns("review"))),
      route("tdcm_test", tdcm_test$ui(ns("tdcm_test"))),
      route(
        "primary_aggregate_results",
        primary_aggregate_results$ui(ns("primary_aggregate_results"))
      ),
      route(
        "primary_individual_results",
        primary_individual_results$ui(ns("primary_individual_results"))
      ),
      route("secondary_results", secondary_results$ui(ns("secondary_results"))),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    data <- reactiveValues(
      ir_matrix = NULL,
      q_matrix = NULL,
      param_specs_data = reactiveValues(),
      model_specs_data = reactiveValues(),
      numAttributes = NULL,
    )

    home$server("home")
    param_specs$server("param_specs", data)
    q_matrix$server("q_matrix", data)
    ir_matrix$server("ir_matrix", data)
    model_specs$server("model_specs", data)
    review$server("review", data)
    tdcm_test$server("tdcm_test", data)
    primary_aggregate_results$server("primary_aggregate_results", data)
    primary_individual_results$server("primary_individual_results", data)
    secondary_results$server("secondary_results", data)
  })
}
