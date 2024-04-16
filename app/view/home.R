box::use(
  shiny[
    moduleServer,
    NS,
    fluidPage,
    titlePanel,
    mainPanel,
    div,
    p,
    fluidRow,
    column,
    br,
    actionButton,
    observeEvent,
    h1,
    reactiveValues
  ],
  shiny.router[change_page],
)

box::use(
  app / view[home, ui_components, param_specs, q_matrix, ir_matrix, model_specs, review],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("TDCMApp", align = "center"),
    br(),
    p("TDCMApp provides a user-friendly
        interface to help you gain valuable
        insights from your test data", align = "center"),
    br(),
    mainPanel(
      fluidRow(
        column(
          6,
          p("Choose Single Group if you have only
            one group of students who took the test.
            Ideal for assessing the overall performance
            of a general student population."),
          br(),
          actionButton(
            inputId = ns("go_to_param_specs"),
            label = "Single Group",
            class = "btn-primary btn-md",
          )
        ),
        column(
          6,
          p("Choose Multi Group when evaluating multiple groups of students who took the test. Especially useful for scenarios like educational interventions, where you may have distinct control and treatment groups."), # nolint: line_length_linter.
          actionButton(
            inputId = ns("button2"),
            label = "Multi Group",
            class = "btn-primary btn-md",
          )
        )
      ),
    )
  )
}

#' @export
server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_to_param_specs, {
      change_page("param_specs")
    })
  })
}
