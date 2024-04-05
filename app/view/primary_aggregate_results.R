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
    datatable
  ],
  datasets[
    mtcars
  ],
  utils[
    write.csv
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
    h2("Primary Aggregate Results"),
    br(),
    textOutput(ns("test")),
    DTOutput(ns("item_params_output")),
    uiOutput(ns("item_params_down_wrapper")),
    DTOutput(ns("growth_output")),
    # plotOutput(ns("tdcmPlot")),
    plotOutput(ns("plot_output")),
    uiOutput(ns("trans_prob_output")),
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      output$test <- renderText(paste("Ready to Fit Model:", is_page("primary_aggregate_results")))

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

        # Then use the reactive expression in other reactive contexts
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

        # Use result() inside your render functions
        output$item_params_output <- renderDT(
          {
            datatable(item_params_result(),
              caption = "Item Parameters",
              rownames = rownames(item_params_result()),
              colnames = colnames(item_params_result()),
            )
          },
          server = FALSE
        )

        output$item_params_down_wrapper <- renderUI({
          downloadButton(ns("item_params_download"), "Download")
        })


        # Add download button
        output$item_params_download <- downloadHandler(
          filename = function() {
            paste("item_parameters.csv", sep = "")
          },
          content = function(file) {
            write.csv(result, file)
          }
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

        output$growth_output <- renderDT({
          datatable(
            growth_result(),
            caption = "Growth Table",
            options = list(scrollX = TRUE)
          )
        })

        output$growth_down_wrapper <- renderUI({
          downloadButton(ns("growth_output_download"), "Download")
        })


        # Add download button
        output$growth_download <- downloadHandler(
          filename = function() {
            paste("growth_output.csv", sep = "")
          },
          content = function(file) {
            write.csv(result, file)
          }
        )

        output$plot_output <- renderPlot(
          {
            plot(mtcars$wt, mtcars$mpg)
          },
          res = 96
        )

        output$plot_output_down_wrapper <- renderUI({
          downloadButton(ns("plot_output_download"), "Download")
        })


        # Add download button
        output$plot_output_download <- downloadHandler(
          filename = function() {
            paste("plot_output.csv", sep = "")
          },
          content = function(file) {
            write.csv(result, file)
          }
        )
        plot_result <- reactive({
          vals <- computed_values()
          tdcm$visualize(data$q_matrix, data$ir_matrix, time_pts, attribute_names, invariance, rule)
        })
        print(plot_result)
        output$tdcmPlot <- renderPlot({
          plot_result() # Call the reactive
        })

        output$plot_result_down_wrapper <- renderUI({
          downloadButton(ns("plot_result_download"), "Download")
        })


        # Add download button
        output$plot_result_download <- downloadHandler(
          filename = function() {
            paste("plot_result.csv", sep = "")
          },
          content = function(file) {
            write.csv(result, file)
          }
        )

        trans_prob_output_result <- reactive({
          vals <- computed_values()
          tdcm$trans_prob(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        output$trans_prob_output <- renderUI({
          table_list <- lapply(1:dim(trans_prob_output_result())[3], function(i) { # nolint
            attribute_title <- dimnames(trans_prob_output_result())[[3]][i]
            renderDT({
              datatable(trans_prob_output_result()[, , i],
                options = list(scrollX = TRUE),
                caption = attribute_title
              )
            })
          })
          tagList(table_list)
        })


        # Hide the spinner when all computations are done
        # NEED TO ADD PLOT TO THIS ONCE WE HAVE IT CLEANED
        observe({
          if (!is.null(item_params_result())
              && !is.null(growth_result())
              && !is.null(trans_prob_output_result())) {
            remove_modal_spinner()
          }
        })
      }
    })

    ui_components$nb_server("nextButton", "primary_individual_results")
  })
}
