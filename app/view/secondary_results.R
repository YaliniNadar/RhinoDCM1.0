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
        downloadHandler
    ],
    shinybusy[
        show_modal_spinner,
        remove_modal_spinner,
    ],
    DT[
        DTOutput,
        renderDT,
        datatable,
        formatRound,
        formatSignif
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
        h2("Secondary Results"),
        br(),
        actionButton(ns("model_fit"), "Model Fit"),
        actionButton(ns("att_corr"), "Attribute Correlation Matrix"),
        actionButton(ns("rel"), "Reliability"),
        uiOutput(ns("model_fit_output")),
        DTOutput(ns("att_corr_output")),
        DTOutput(ns("rel_output")),
        ui_components$next_button(ns("nextButton")),
        ui_components$back_button(ns("backButton")),
    )
}

#' @export
server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$model_fit, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$model_fit(data$q_matrix, data$ir_matrix, time_pts, attribute_names)

            # Create a data.table containing specific elements
            misc_data <- tdcm$get_misc_datatable(result)

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

            # Render Global Fit Stats data frame
            output$global_fit_stats <- renderDT({
                formatted_table <- formatSignif(
                    datatable(result$Global.Fit.Stats, options = list(scrollX = TRUE)),
                    1,
                    2
                )
            })

            # Render Item Pairs data frame
            output$item_pairs <- renderDT({
                formatted_table <- formatRound(
                    formatRound(
                        datatable(result$Item.Pairs, options = list(scrollX = TRUE)),
                        columns = 3:7,
                        digits = 0
                    ),
                    columns = 8:18,
                    digits = 2,
                )
            })

            # Render Gloabl Fit Tests data frame
            output$global_fit_tests <- renderDT({
                datatable(result$Global.Fit.Tests,
                    options = list(scrollX = TRUE)
                )
            })

            # Render Global Fit Stats data frame
            output$global_fit_stats2 <- renderDT({
                dt <- result$Global.Fit.Stats2
                dt_transposed <- as.data.frame(t(dt))
                colnames(dt_transposed) <- c("Value")
                datatable(dt_transposed,
                    options = list(scrollX = TRUE)
                )
            })

            # Render Item RMSEA table
            output$item_rmsea <- renderDT({
                item_rmsea_dt <- tdcm$convert_to_datatable(result$Item.RMSEA)
                datatable(item_rmsea_dt,
                    options = list(scrollX = TRUE)
                )
            })

            # Render Misc data table
            output$misc_table <- renderDT({
                datatable(misc_data,
                    options = list(scrollX = TRUE)
                )
            })

            remove_modal_spinner()
        })

        observeEvent(input$att_corr, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$att_corr(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )
            output$att_corr_output <- renderDT({
                datatable(
                    result,
                    caption = "Attribute Correlation",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })

        observeEvent(input$rel, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$reliability(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )
            output$rel_output <- renderDT({
                datatable(
                    result,
                    caption = "Reliability",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })


        ui_components$nb_server("nextButton", "/")
    })
}
