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
    uiOutput(ns("att_class_result_down_wrapper")),
    DTOutput(ns("most_likely_trans_output")),
    uiOutput(ns("most_likely_trans_down_wrapper")),
    DTOutput(ns("trans_pos_output")),
    uiOutput(ns("trans_pos_output_down_wrapper")),
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

        output$att_class_result_down_wrapper <- renderUI({
          downloadButton(ns("att_class_result_download"), "Download")
        })

        # Add download button
        output$att_class_result_download <- downloadHandler(
          filename = function() {
            paste("att_class_result.csv", sep = "")
          },
          content = function(attClassFile) {
            write.csv(att_class_result(), attClassFile)
          }
        )

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

        output$most_likely_trans_down_wrapper <- renderUI({
          downloadButton(ns("most_likely_trans_output_download"), "Download")
        })

        # Add download button
        output$most_likely_trans_output_download <- downloadHandler(
          filename = function() {
            paste("most_likely_trans.csv", sep = "")
          },
          content = function(mostLikelyTransFile) {
            write.csv(most_likely_trans_result(), mostLikelyTransFile)
          }
        )

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

        output$trans_pos_output_down_wrapper <- renderUI({
          downloadButton(ns("trans_pos_output_download"), "Download")
        })

        # Add download button
        output$trans_pos_output_download <- downloadHandler(
          filename = function() {
            paste("trans_pos.csv", sep = "")
          },
          content = function(transPosFile) {
            write.csv(trans_pos_output_result(), transPosFile)
          }
        )
      }
    })

    ui_components$nb_server("nextButton", "secondary_results")
  })
}
