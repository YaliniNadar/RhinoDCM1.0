box::use(
  shiny[
    NS,
    fluidPage,
    tabsetPanel,
    tabPanel,
    br,
    h1,
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
    renderText,
    tableOutput,
    textOutput,
    plotOutput,
    renderDataTable,
    renderUI,
    uiOutput,
    tagList,
    downloadButton,
    downloadHandler,
    reactive,
    div,
    tags,
    a
  ],
  shinybusy[
    show_modal_spinner,
    remove_modal_spinner,
  ],
  shiny.router[is_page, router_ui, router_server, route, route_link],
  DT[
    DTOutput,
    renderDT,
    datatable,
    JS
  ],
  datasets[
    mtcars
  ],
)

box::use(
  app / view[
    ui_components,
    primary_aggregate_results,
    primary_individual_results,
    secondary_results,
    table_helper
  ],
  app / logic[tdcm],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Primary Individual Results"),
    tagList(
      tabsetPanel(
        id = ns("output_tabs"),
        tabPanel(a("Primary Aggregate Results", href = route_link("primary_aggregate_results"))),
        tabPanel(a("Primary Individual Results", href = route_link("primary_individual_results"))),
        tabPanel(a("Secondary Results", href = route_link("secondary_results")))
      )
    ),
    # router_ui(
    #   route("primary_aggregate_results", primary_individual_results$ui(ns("primary_aggregate_results"))),
    #   route("primary_individual_results", primary_individual_results$ui(ns("primary_individual_results"))),
    #   route("secondary_results", secondary_results$ui(ns("secondary_results")))
    # ),
    br(),
    DTOutput(ns("classification_output")),
    uiOutput(ns("att_class_result_down_wrapper")),
    DTOutput(ns("most_likely_trans_output")),
    uiOutput(ns("most_likely_trans_down_wrapper")),
    DTOutput(ns("trans_pos_output")),
    uiOutput(ns("trans_pos_output_down_wrapper")),
    br(),
    br(),
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
    br(),
    br(),
    br(),
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

        output$classification_output <- renderDT(
          {
            datatable(
              att_class_result(),
              caption = "Attribute Classification",
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                searching = FALSE,
                initComplete = JS(table_helper$format_pagination())
              ),
              autoHideNavigation = TRUE,
            )
          },
          server = FALSE
        )

        output$att_class_result_down_wrapper <- renderUI({
          downloadButton(ns("att_class_result_download"), "Download")
        })

        # Add download button
        output$att_class_result_download <-
          table_helper$create_download_handler(
            att_class_result(),
            "attribute_classification.xlsx"
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

        output$most_likely_trans_output <- renderDT(
          {
            datatable(
              most_likely_trans_result()[, -1],
              caption = "Most Likely Transitions",
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                searching = FALSE,
                initComplete = JS(table_helper$format_pagination())
              ),
              autoHideNavigation = TRUE,
            )
          },
          server = FALSE
        )

        output$most_likely_trans_down_wrapper <- renderUI({
          downloadButton(ns("most_likely_trans_output_download"), "Download")
        })

        # Add download button
        output$most_likely_trans_output_download <-
          table_helper$create_download_handler(
            most_likely_trans_result(),
            "most_likely_transitions.xlsx"
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

        output$trans_pos_output <- renderDT(
          {
            datatable(
              trans_pos_output_result(),
              caption = "Transition Position",
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                searching = FALSE,
                initComplete = JS(table_helper$format_pagination())
              ),
              autoHideNavigation = TRUE,
            )
          },
          server = FALSE
        )

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
        output$trans_pos_output_download <-
          table_helper$create_download_handler(
            trans_pos_output_result(),
            "transition_position.xlsx"
          )
      }
    })

    ui_components$nb_server("nextButton", "secondary_results")
  })
}
