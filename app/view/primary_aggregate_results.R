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
        h2("Primary Aggregate Results"),
        br(),
        actionButton(ns("trans_prob"), "Transition Probabilities"),
        uiOutput(ns("trans_prob_output")),
        ui_components$next_button(ns("nextButton")),
        ui_components$back_button(ns("backButton")),
    )
}

#' @export
server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        observeEvent(input$trans_prob, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$trans_prob(data$q_matrix, data$ir_matrix, time_pts, attribute_names)

            output$trans_prob_output <- renderUI({
                table_list <- lapply(1:dim(result)[3], function(i) { # nolint: seq_linter.
                    attribute_title <- dimnames(result)[[3]][i]
                    renderDT({
                        datatable(result[, , i],
                            options = list(scrollX = TRUE),
                            caption = attribute_title
                        )
                    })
                })
                tagList(table_list)
            })

            remove_modal_spinner()
        })

        ui_components$nb_server("nextButton", "primary_individual_results")
    })
}
