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
    app / view[ui_components],
    app / logic / tdcm
)

#' @export
ui <- function(id) {
    ns <- NS(id)

    fluidPage(
        h2("Basic Summary"),
        br(),
        actionButton(ns("item_params"), "Item Parameters Table"),
        actionButton(ns("growth_table"), "Growth Table"),
        actionButton(ns("plot"), "Proficiency Proportion Plots *"),
        DTOutput(ns("item_params_output")),
        uiOutput(ns("item_params_down_wrapper")),
        DTOutput(ns("growth_output")),
        # plotOutput(ns("tdcmPlot")),
        plotOutput(ns("plot_output"), click = "plot_click"),
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
            attribute_names <- data$review$col_names
            time_pts <- data$param_specs_data$num_time_points

            result <- tdcm$item_parameters(data$q_matrix, data$ir_matrix, time_pts, attribute_names)
            output$item_params_output <- renderDT(
                {
                    datatable(result,
                        caption = "Item Parameters",
                        rownames = rownames(result),
                        colnames = colnames(result),
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


            remove_modal_spinner()
        })

        observeEvent(input$growth_table, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$growth(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )

            output$growth_output <- renderDT({
                datatable(
                    result,
                    caption = "Growth Table",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })

        observeEvent(input$plot, {
            show_modal_spinner(spin = "fading-circle")
            output$plot_output <- renderPlot(
                {
                    plot(mtcars$wt, mtcars$mpg)
                },
                res = 96
            )
            remove_modal_spinner()
        })

        observeEvent(input$plot, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$visualize(data$q_matrix, data$ir_matrix, time_pts, attribute_names)
            print(result)
            output$tdcmPlot <- renderPlot(result)
            remove_modal_spinner()
        })

        ui_components$nb_server("nextButton", "primary_aggregate_results")
    })
}
