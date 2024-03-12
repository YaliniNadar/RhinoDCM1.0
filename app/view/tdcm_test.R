box::use(
  shiny[
    NS,
    fluidPage,
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
    dataTableOutput,
    renderDataTable,
    renderUI,
    uiOutput
  ],
  shinybusy[
    show_modal_spinner,
    remove_modal_spinner,
  ],
  DT[DTOutput, renderDT, datatable],
)

box::use(
  app/view[ui_components],
  app/logic/tdcm
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("TDCM test"),
    br(),

    actionButton(ns("item_params"), "Item Parameters Table"),
    actionButton(ns("growth_table"), "Growth Table"),
    actionButton(ns("plot"), "Proficiency Proportion Plots"),
    actionButton(ns("trans_prob"), "Transition Probabilities"),

    DTOutput(ns("item_params_output")),
    # dataTableOutput(ns("item_params_output")),
    DTOutput(ns("growth_output")),
    uiOutput(ns("trans_prob_output")),

    plotOutput(ns("tdcmPlot")),

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
      attribute_names_vector <- unlist(strsplit(data$param_specs_data$attribute_names, ","))

      result <- tdcm$item_parameters(data$q_matrix, data$ir_matrix)
      output$item_params_output <- renderDT({
        datatable(result,
                  rownames = rownames(result),
                  colnames = colnames(result), )
      }, server = FALSE)
      # output$item_params_output <- renderDataTable(result)
      remove_modal_spinner()
    })

    observeEvent(input$growth_table, {
      show_modal_spinner(spin = "fading-circle")
      result <- tdcm$growth(data$q_matrix,
                            data$ir_matrix)
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
      result <- tdcm$visualize(data$q_matrix,
                               data$ir_matrix,
                              )
      print(result)
      output$tdcmPlot <- renderPlot(result)
      remove_modal_spinner()
    })

    observeEvent(input$trans_prob, {
      show_modal_spinner(spin = "fading-circle")
      result <- tdcm$trans_prob(data$q_matrix,
                                data$ir_matrix)

      output$trans_prob_output <- renderUI({
        # Create an empty list to store DT::renderDataTable objects
        table_list <- vector("list", dim(result)[3])

        # Iterate through each matrix and create a renderDataTable object
        for (i in 1:dim(result)[3]) {
          table_list[[i]] <- renderDT({
            datatable(result[,,i],
                      options = list(scrollX = TRUE),
                      caption = paste("Matrix", i, ": Time 1 to Time 2"))
          })
        }

        # Return the list of renderDataTable objects
        table_list
      })

      remove_modal_spinner()
    })


    ui_components$nb_server("nextButton", "/")
  })
}