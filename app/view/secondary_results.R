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
    reactive,
    showModal,
    modalDialog,
    modalButton,
    div,
    tags,
    a
  ],
  shinybusy[
    show_modal_spinner,
    remove_modal_spinner,
  ],
  shiny.router[
    is_page,
    change_page,
    router_ui,
    router_server,
    route,
    route_link
  ],
  DT[
    DTOutput,
    renderDT,
    datatable,
    JS,
    formatRound
  ],
  datasets[
    mtcars
  ],
)

box::use(
  app/view[
    ui_components,
    format_table,
    primary_aggregate_results,
    primary_individual_results,
    secondary_results
  ],
  app/logic[tdcm, table]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Secondary Results"),
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
    uiOutput(ns("model_fit_output")),
    uiOutput(ns("model_fit_result_down_wrapper")),
    DTOutput(ns("att_corr_output")),
    uiOutput(ns("att_corr_result_down_wrapper")),
    DTOutput(ns("rel_output")),
    uiOutput(ns("reli_result_down_wrapper")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      if (is_page("primary_aggregate_results")) {
        show_modal_spinner(spin = "fading-circle")
        computedValues <- reactive({
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

        model_fit_result <- reactive({
          vals <- computedValues()
          tdcm$model_fit(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })

        output$model_fit_output <- renderUI({
          tagList(
            tabsetPanel(
              tabPanel("Global Fit Stats", DTOutput(ns("global_fit_stats"))),
              tabPanel("Item Pairs", DTOutput(ns("item_pairs"))),
              tabPanel("Global Fit Tests", DTOutput(ns("global_fit_tests"))),
              tabPanel("Global Fit Stats 2", DTOutput(ns("global_fit_stats2"))),
              tabPanel("Item RMSEA", DTOutput(ns("item_rmsea"))),
              tabPanel("Misc", DTOutput(ns("misc_table"))),
              # Add more tabPanels for other elements as needed
            )
          )
        })

        check_columns_for_rounding <- function(data) {
          # Check if columns need rounding
          columns_to_round <- which(sapply(data, function(x) {
            is.numeric(x) && any(abs(x - round(x, 3)) > 0)
          }))
          return(columns_to_round)
        }

        # Render Global Fit Stats data frame
        output$global_fit_stats <- renderDT({
          columns_to_round <- check_columns_for_rounding(model_fit_result()$Global.Fit.Stats)
          formatted_table <- formatRound(
            datatable(model_fit_result()$Global.Fit.Stats,
              options = list(
                scrollX = TRUE,
                dom = "t",
                initComplete = JS(format_table$format_pagination())
              )
            ),
            columns = columns_to_round,
            digits = 3
          )
        })

        # Render Item Pairs data frame
        output$item_pairs <- renderDT({
          columns_to_round <- check_columns_for_rounding(model_fit_result()$Item.Pairs)
          formatted_table <- formatRound(
            formatRound(
              datatable(model_fit_result()$Item.Pairs,
                options = list(
                  scrollX = TRUE,
                  searching = FALSE,
                  initComplete = JS(format_table$format_pagination())
                )
              ),
              columns = columns_to_round,
              digits = 3
            ),
            columns = c(3:7),
            digits = 0
          )
        })

        # Render Gloabl Fit Tests data frame
        output$global_fit_tests <- renderDT({
          columns_to_round <- check_columns_for_rounding(model_fit_result()$Global.Fit.Tests)
          formatted_table <- formatRound(
            datatable(model_fit_result()$Global.Fit.Tests,
              options = list(
                scrollX = TRUE,
                dom = "t",
                initComplete = JS(format_table$format_pagination())
              )
            ),
            columns = columns_to_round,
            digits = 3
          )
        })

        # Render Global Fit Stats 2 data frame
        output$global_fit_stats2 <- renderDT({
          dt <- model_fit_result()$Global.Fit.Stats2
          dt_transposed <- as.data.frame(t(dt))
          colnames(dt_transposed) <- c("Value")

          columns_to_round <- check_columns_for_rounding(dt_transposed)
          formatted_table <- formatRound(
            datatable(dt_transposed,
              options = list(
                scrollX = TRUE,
                dom = "t",
                initComplete = JS(format_table$format_pagination())
              )
            ),
            columns = columns_to_round,
            digits = 3
          )
        })

        # Create a data.table for Item RMSEA table
        item_rmsea_dt <- reactive({
          tdcm$convert_to_datatable(model_fit_result()$Item.RMSEA)
        })

        # Render Item RMSEA table
        output$item_rmsea <- renderDT({
          columns_to_round <- check_columns_for_rounding(item_rmsea_dt)
          formatted_table <- formatRound(
            datatable(item_rmsea_dt(),
              options = list(
                scrollX = TRUE,
                searching = FALSE,
                initComplete = JS(format_table$format_pagination())
              )
            ),
            columns = 2,
            digits = 3
          )
        })

        # Create a data.table containing specific elements
        misc_data <- reactive({
          tdcm$get_misc_datatable(model_fit_result())
        })

        # Render Misc data table
        output$misc_table <- renderDT({
          columns_to_round <- check_columns_for_rounding(misc_data)
          print(columns_to_round)
          formatted_table <- formatRound(
            datatable(misc_data(),
              options = list(
                scrollX = TRUE,
                dom = "t",
                initComplete = JS(format_table$format_pagination())
              )
            ),
            columns = 2,
            digits = 3
          )
        })

        output$model_fit_result_down_wrapper <- renderUI({
          downloadButton(ns("model_fit_download"), "Download")
        })

        # Add download button
        data_tables <- list(
          list(name = "Global Fit Stats", data = model_fit_result()$Global.Fit.Stats),
          list(name = "Item Pairs", data = model_fit_result()$Item.Pairs),
          list(name = "Global Fit Tests", data = model_fit_result()$Global.Fit.Tests),
          list(name = "Global Fit Stats 2", data = model_fit_result()$Global.Fit.Stats2),
          list(name = "Item RMSEA", data = item_rmsea_dt()),
          list(name = "Misc Data", data = misc_data())
        )

        output$model_fit_download <- downloadHandler(
          filename = function() {
            "model_fit.xlsx"
          },
          content = function(file) {
            table$write_multiple_sheets(data_tables, file)
          }
        )

        att_corr_result <- reactive({
          vals <- computedValues()
          tdcm$att_corr(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })
        output$att_corr_output <- renderDT({
          datatable(
            att_corr_result(),
            caption = "Attribute Correlation",
            options = list(
              scrollX = TRUE,
              searching = FALSE,
              initComplete = JS(format_table$format_pagination())
            )
          )
        })

        output$att_corr_result_down_wrapper <- renderUI({
          downloadButton(ns("att_result_download"), "Download")
        })

        # Add download button
        output$att_result_download <-
          ui_components$create_download_handler(
            att_corr_result(),
            "attribute_correlation.xlsx"
          )

        reli_result <- reactive({
          vals <- computedValues()
          tdcm$reliability(
            data$q_matrix,
            data$ir_matrix,
            vals$time_pts,
            vals$attribute_names,
            vals$invariance,
            vals$rule
          )
        })
        output$rel_output <- renderDT({
          datatable(
            reli_result(),
            caption = "Reliability",
            options = list(
              scrollX = TRUE,
              searching = FALSE,
              initComplete = JS(format_table$format_pagination())
            )
          )
        })
        output$reli_result_down_wrapper <- renderUI({
          downloadButton(ns("reli_result_download"), "Download")
        })

        # Add download button
        output$reli_result_download <-
          ui_components$create_download_handler(
            reli_result(),
            "reliability.xlsx"
          )

        # Hide the spinner when all computations are done
        observe({
          if (!is.null(model_fit_result()) &&
            !is.null(att_corr_result()) &&
            !is.null(reli_result())) {
            remove_modal_spinner()
          }
        })
      }
    })
  })
}
