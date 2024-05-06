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
    a,
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
    h2("Primary Aggregate Results"),
    tagList(
      tabsetPanel(
        id = ns("output_tabs"),
        tabPanel(a("Primary Aggregate Results", href = route_link("primary_aggregate_results"))),
        tabPanel(a("Primary Individual Results", href = route_link("primary_individual_results"))),
        tabPanel(a("Secondary Results", href = route_link("secondary_results")))
      )
    ),
    router_ui(
      route(
        "primary_individual_results",
        primary_individual_results$ui(ns("primary_individual_results"))
      ),
      route(
        "secondary_results",
        secondary_results$ui(ns("secondary_results"))
      )
    ),
    br(),
    uiOutput(ns("dynamic_content")),
    DTOutput(ns("item_params_output")),
    uiOutput(ns("item_params_down_wrapper")),
    br(),
    br(),
    DTOutput(ns("growth_output")),
    br(),
    uiOutput(ns("growth_down_wrapper")),
    br(),
    br(),
    plotOutput(ns("tdcmLinePlot")),
    br(),
    uiOutput(ns("tdcmLinePlot_down_wrapper")),
    br(),
    br(),
    plotOutput(ns("tdcmBarPlot")),
    br(),
    uiOutput(ns("tdcmBarPlot_down_wrapper")),
    br(),
    br(),
    uiOutput(ns("trans_prob_output")),
    br(),
    uiOutput(ns("trans_prob_down_wrapper")),
    br(),
    br(),
    ui_components$next_button(ns("nextButton")),
    ui_components$reset_button(ns("resetBtn")),
    br(),
    br(),
    br(),
  )
}

#' @export
server <- function(id, data, input, output) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      if (is_page("primary_aggregate_results")) {
        show_modal_spinner(spin = "fading-circle")
        # Define a reactive expression that contains all needed data
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

        item_params_result <- reactive({
          # Access the values from computedValues() reactive expression
          vals <- computed_values() # This is now a reactive access
          tdcm$item_parameters(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        # Render Item Parameters output
        output$item_params_output <- renderDT(
          {
            datatable(item_params_result(),
              caption = "Item Parameters",
              rownames = rownames(item_params_result()),
              colnames = colnames(item_params_result()),
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

        # Download Button
        output$item_params_down_wrapper <- renderUI({
          downloadButton(ns("item_params_download"), "Download")
        })

        # Download Handler for item parameters
        output$item_params_download <- table_helper$create_download_handler(
          item_params_result(),
          "item_parameters.xlsx"
        )

        growth_result <- reactive({
          vals <- computed_values() # Correctly access the computed values here
          tdcm$growth(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts, # Use the values from 'vals' here
            vals$attribute_names, # Use the values from 'vals' here
            vals$invariance, # Use the values from 'vals' here
            vals$rule # Use the values from 'vals' here
          )
        })

        # Render Growth Table output
        output$growth_output <- renderDT(
          {
            datatable(
              growth_result(),
              caption = "Growth Table",
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

        # Download Button
        output$growth_down_wrapper <- renderUI({
          downloadButton(ns("growth_output_download"), "Download")
        })

        output$growth_output_download <- table_helper$create_download_handler(
          growth_result(),
          "growth_table.xlsx"
        )

        # Render Line Plot
        line_plot_result <- reactive({
          vals <- computed_values() # Correctly access the computed values here
          tdcm$visualize(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule,
            type = "line"
          )
        })

        # Render Line Plot
        line_plot_result2 <- reactive({
          vals <- computed_values() # Correctly access the computed values here
          tdcm$visualize(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule,
            type = "line"
          )
        })
        output$tdcmLinePlot <- renderPlot({
          line_plot_result() # Call the reactive
        })

        # Download Button
        output$tdcmLinePlot_down_wrapper <- renderUI({
          downloadButton(ns("tdcmLinePlot_download"), "Download")
        })

        output$tdcmLinePlot_download <- table_helper$create_image_download_handler(
          line_plot_result2(),
          "line_plot.png"
        )

        # Render Bar Plot
        bar_plot_result <- reactive({
          vals <- computed_values() # Correctly access the computed values here
          tdcm$visualize(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule,
            type = "bar"
          )
        })

        # Render Bar Plot
        bar_plot_result2 <- reactive({
          vals <- computed_values()
          tdcm$visualize(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule,
            type = "bar"
          )
        })

        output$tdcmBarPlot <- renderPlot({
          bar_plot_result() # Call the reactive
        })

        # Download Button
        output$tdcmBarPlot_down_wrapper <- renderUI({
          downloadButton(ns("tdcmBarPlot_download"), "Download")
        })

        output$tdcmBarPlot_download <- table_helper$create_image_download_handler(
          bar_plot_result2(),
          "bar_plot.png"
        )

        trans_prob_output_result <- reactive({
          vals <- computed_values() # Correctly access the computed values here
          tdcm$trans_prob(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        # Render Transition Probability output
        output$trans_prob_output <- renderUI({
          table_list <- lapply(1:2, function(row) {
            fluidRow(
              lapply(1:2, function(col) {
                index <- (row - 1) * 2 + col
                if (index <= dim(trans_prob_output_result())[3]) {
                  attribute_title <- dimnames(trans_prob_output_result())[[3]][index]
                  column(
                    width = 6,
                    renderDT(
                      {
                        datatable(trans_prob_output_result()[, , index],
                          options = list(
                            scrollX = TRUE,
                            pageLength = 10,
                            searching = FALSE,
                            initComplete = JS(table_helper$format_pagination())
                          ),
                          autoHideNavigation = TRUE,
                          caption = attribute_title
                        )
                      },
                      server = FALSE
                    )
                  )
                }
              })
            )
          })
          tagList(table_list)
        })

        # Download Button
        output$trans_prob_down_wrapper <- renderUI({
          downloadButton(ns("trans_prob_result_download"), "Download")
        })

        output$trans_prob_result_download <- table_helper$create_download_handler(
          trans_prob_output_result(),
          "transition_probabilities.xlsx"
        )

        # Hide the spinner when all computations are done
        # NEED TO ADD PLOT TO THIS ONCE WE HAVE IT CLEANED
        observe({
          if (!is.null(item_params_result()) &&
            !is.null(growth_result()) &&
            !is.null(trans_prob_output_result())) {
            remove_modal_spinner()
          }
        })
      }
    })
    ui_components$rb_server("resetBtn")
    ui_components$nb_server("nextButton", "primary_individual_results")
  })
}
