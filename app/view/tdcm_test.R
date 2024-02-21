box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    h4,
    fluidRow,
    column,
    actionButton,
    observeEvent,
    moduleServer,
    observe,
    renderText,
    textOutput,
  ]
)

box::use(
  app/view[ui_components],
  app/logic/tdcm
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("TDCM test"),
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
    ns <- session$ns

    observeEvent(input$testButton, {
      # print(data$param_specs_data$num_time_points)
      result <- tdcm$tdcm_test(data$q_matrix, data$ir_matrix)
      output$resultOutput <- renderText(result)
    })

    ui_components$nb_server("nextButton", "/")
  })
}