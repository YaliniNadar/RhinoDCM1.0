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
    renderTable,
    renderPlot,
    tableOutput,
    textOutput,
    plotOutput,
  ],
  shinybusy[
    show_modal_spinner,
    remove_modal_spinner,
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

    actionButton(ns("item_params"), "Item Parameters Table"),
    actionButton(ns("growth_table"), "Growth Table"),
    actionButton(ns("plot"), "Proficiency Proportion Plots"),

    tableOutput(ns("resultOutput")),

    plotOutput(ns("tdcmPlot")),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$item_params, {
      show_modal_spinner(spin = "fading-circle")
      # print(data$param_specs_data$num_time_points)
      result <- tdcm$item_parameters(data$q_matrix, data$ir_matrix)
      output$resultOutput <- renderTable(result)
      remove_modal_spinner()
    })

    observeEvent(input$growth_table, {
      show_modal_spinner(spin = "fading-circle")
      result <- tdcm$growth(data$q_matrix, data$ir_matrix)
      output$resultOutput <- renderTable(result)
      remove_modal_spinner()
    })

    observeEvent(input$plot, {
      show_modal_spinner(spin = "fading-circle")
      result <- tdcm$visualize(data$q_matrix, data$ir_matrix)
      print(result)
      output$tdcmPlot <- renderPlot(result)
      remove_modal_spinner()
    })

    ui_components$nb_server("nextButton", "/")
  })
}