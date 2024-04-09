box::use(
  shiny[
    NS,
    fluidPage,
    tabsetPanel,
    tabPanel,
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
    renderDataTable,
    renderUI,
    uiOutput,
    tagList,
    downloadButton,
    downloadHandler,
    reactive
  ],
  shinybusy[
    show_modal_spinner,
    remove_modal_spinner,
  ],
  shiny.router[is_page],
  DT[
    DTOutput,
    renderDT,
    datatable,
    formatRound,
    formatSignif,
    JS
  ],
  datasets[
    mtcars
  ],
  utils[
    write.csv
  ]
)

box::use(
  app / view[ui_components],
  app / logic / tdcm
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Primary Individual Results"),
    br(),
    DTOutput(ns("classification_output")),
    DTOutput(ns("most_likely_trans_output")),
    DTOutput(ns("trans_pos_output")),
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      if (is_page("primary_individual_results")) {
        computed_values <- reactive({
          # Access reactive values here
          attribute_names <- data$review$col_names
          time_pts <- data$param_specs_data$num_time_points
          invariance <- data$model_specs_data$itemParameter
          rule <- data$model_specs_data$dcmEstimate

          # Return a list of all computed values
          list(
            attribute_names = attribute_names,
            time_pts = time_pts,
            invariance = invariance,
            rule = rule
          )
        })
        att_class_result <- reactive({
          vals <- computed_values() # This is now a reactive access
          tdcm$att_class(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        output$classification_output <- renderDT({
          datatable(
            att_class_result(),
            caption = "Attribute Classification",
            options = list(
              scrollX = TRUE,
              initComplete = JS(ui_components$format_pagination())
            )
          )
        })

        most_likely_trans_result <- reactive({
          vals <- computed_values() # This is now a reactive access
          tdcm$most_likely_trans(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        output$most_likely_trans_output <- renderDT({
          datatable(
            most_likely_trans_result()[, -1],
            caption = "Most Likely Transitions",
            options = list(
              scrollX = TRUE,
              initComplete = JS(ui_components$format_pagination())
            )
          )
        })

        trans_pos_output_result <- reactive({
          vals <- computed_values() # This is now a reactive access
          tdcm$trans_pos(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        output$trans_pos_output <- renderDT({
          datatable(
            trans_pos_output_result(),
            caption = "Transition Position",
            options = list(
              scrollX = TRUE,
              initComplete = JS(ui_components$format_pagination())
            )
          )
        })
      }
    })

    ui_components$nb_server("nextButton", "secondary_results")
  })
}
