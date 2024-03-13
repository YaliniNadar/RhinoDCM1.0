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
    renderDataTable,
    renderUI,
    uiOutput,
    tagList,
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
    actionButton(ns("plot"), "Proficiency Proportion Plots *"),
    actionButton(ns("trans_prob"), "Transition Probabilities"),
    actionButton(ns("attr_class"), "Attribute Classification"),
    actionButton(ns("most_likely_trans"), "Most Likely Transitions *"),
    actionButton(ns("trans_pos"), "Transition Position"),
    actionButton(ns("model_fit"), "Model Fit"),

    DTOutput(ns("item_params_output")),
    DTOutput(ns("growth_output")),
    plotOutput(ns("tdcmPlot")),

    uiOutput(ns("trans_prob_output")),

    DTOutput(ns("classification_output")),
    DTOutput(ns("most_likely_trans_output")),
    DTOutput(ns("trans_pos_output")),

    uiOutput(ns("model_fit_output")),



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
      time_pts <- data$param_specs_data$num_time_points

      result <- tdcm$item_parameters(data$q_matrix, data$ir_matrix, time_pts)
      output$item_params_output <- renderDT({
        datatable(result,
                  caption = "Item Parameters",
                  rownames = rownames(result),
                  colnames = colnames(result), )
      }, server = FALSE)
      remove_modal_spinner()
    })

    observeEvent(input$growth_table, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$growth(data$q_matrix,
                            data$ir_matrix,
                            time_pts)
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
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$visualize(data$q_matrix, data$ir_matrix, time_pts)
      print(result)
      output$tdcmPlot <- renderPlot(result)
      remove_modal_spinner()
    })

    observeEvent(input$trans_prob, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$trans_prob(data$q_matrix, data$ir_matrix, time_pts)
      print(result)

      output$trans_prob_output <- renderUI({
        table_list <- lapply(1:dim(result)[3], function(i) {
          attribute_title <- dimnames(result)[[3]][i]
          renderDT({
            datatable(result[, , i],
                      options = list(scrollX = TRUE),
                      caption = attribute_title)
          })
        })
        tagList(table_list)
      })

      remove_modal_spinner()
    })

    observeEvent(input$attr_class, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$att_class(data$q_matrix,
                               data$ir_matrix,
                               time_pts)
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
      result <- tdcm$most_likely_trans(data$q_matrix,
                                       data$ir_matrix,
                                       time_pts)
      output$most_likely_trans_output <- renderDT({
        datatable(
          result,
          caption = "Most Likely Transitions",
          options = list(scrollX = TRUE)
        )
      })
      remove_modal_spinner()
    })

    observeEvent(input$trans_pos, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$trans_pos(data$q_matrix,
                               data$ir_matrix,
                               time_pts)
      output$trans_pos_output <- renderDT({
        datatable(
          result,
          caption = "Transition Position",
          options = list(scrollX = TRUE)
        )
      })
      remove_modal_spinner()
    })

    observeEvent(input$model_fit, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      result <- tdcm$model_fit(data$q_matrix, data$ir_matrix, time_pts)
      print(result)

      # output$model_fit_output <- renderUI({
      #   table_list <- lapply(1:dim(result)[12], function(i) {
      #     attribute_title <- dimnames(result)[[12]][i]
      #     renderDT({
      #       datatable(result[, , i],
      #                 options = list(scrollX = TRUE),
      #                 caption = attribute_title)
      #     })
      #   })
      #   tagList(table_list)
      # })

      remove_modal_spinner()
    })



    ui_components$nb_server("nextButton", "/")
  })
}