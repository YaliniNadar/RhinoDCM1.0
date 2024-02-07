box::use(
  shiny[
    moduleServer,
    NS,
    fluidPage,
    titlePanel,
    mainPanel,
    h1,
    p,
    fluidRow,
    column,
    br,
    actionButton,
    observeEvent
  ],
  shiny.router[change_page],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    mainPanel(
      h1("TDCM", align = "left"),
      p("TDCM provides a user-friendly
        interface to help you gain valuable
        insights from your test data"),
      br(),
      fluidRow(
        column(
          6,
          p("Choose this option if you have only
            one group of students who took the test.
            Ideal for assessing the overall performance
            of a general student population."),
          br(),
          br(),
          actionButton(
            inputId = ns("go_to_param_specs"),
            label = "Single Group",
            class = "btn-primary btn-md"
          )
        ),
        column(
          6,
          p("Choose this option when evaluating multiple groups of students who took the test. Especially useful for scenarios like educational interventions, where you may have distinct control and treatment groups."), # nolint: line_length_linter.
          br(),
          actionButton(
            inputId = ns("button2"),
            label = "Multi Group",
            class = "btn-primary btn-md"
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
