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
        h2("Primary Individual Results"),
        br(),
        actionButton(ns("attr_class"), "Attribute Classification"),
        actionButton(ns("most_likely_trans"), "Most Likely Transitions"),
        actionButton(ns("trans_pos"), "Transition Position"),
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
        observeEvent(input$attr_class, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$att_class(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )

            output$classification_output <- renderDT({
                datatable(
                    result,
                    caption = "Attribute Classification",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })

        observeEvent(input$most_likely_trans, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$most_likely_trans(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )

            output$most_likely_trans_output <- renderDT({
                datatable(
                    result[, -1],
                    caption = "Most Likely Transitions",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })

        observeEvent(input$trans_pos, {
            show_modal_spinner(spin = "fading-circle")
            time_pts <- data$param_specs_data$num_time_points
            attribute_names <- data$review$col_names

            result <- tdcm$trans_pos(
                data$q_matrix,
                data$ir_matrix,
                time_pts,
                attribute_names
            )

            output$trans_pos_output <- renderDT({
                datatable(
                    result,
                    caption = "Transition Position",
                    options = list(scrollX = TRUE)
                )
            })
            remove_modal_spinner()
        })

        ui_components$nb_server("nextButton", "secondary_results")
    })
}
