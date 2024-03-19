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
    actionButton(ns("most_likely_trans"), "Most Likely Transitions"),
    actionButton(ns("trans_pos"), "Transition Position"),
    actionButton(ns("model_fit"), "Model Fit"),
    actionButton(ns("att_corr"), "Attribute Correlation Matrix"),
    actionButton(ns("rel"), "Reliability"),

    DTOutput(ns("item_params_output")),
    uiOutput(ns("item_params_down_wrapper")),

    DTOutput(ns("growth_output")),
    # plotOutput(ns("tdcmPlot")),
    plotOutput(ns("plot_output"), click = "plot_click"),

    uiOutput(ns("trans_prob_output")),

    DTOutput(ns("classification_output")),
    DTOutput(ns("most_likely_trans_output")),
    DTOutput(ns("trans_pos_output")),

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
    observeEvent(input$item_params, {
      show_modal_spinner(spin = "fading-circle")
      attribute_names <- data$param_specs_data$attribute_names
      time_pts <- data$param_specs_data$num_time_points

      result <- tdcm$item_parameters(data$q_matrix, data$ir_matrix, time_pts, attribute_names)
      output$item_params_output <- renderDT({
        datatable(result,
                  caption = "Item Parameters",
                  rownames = rownames(result),
                  colnames = colnames(result), )
      }, server = FALSE)

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
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$growth(data$q_matrix,
                            data$ir_matrix,
                            time_pts,
                            attribute_names)

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
      output$plot_output <- renderPlot({
        plot(mtcars$wt, mtcars$mpg)
      }, res = 96)
      remove_modal_spinner()
    })

    observeEvent(input$plot, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$visualize(data$q_matrix, data$ir_matrix, time_pts, attribute_names)
      print(result)
      output$tdcmPlot <- renderPlot(result)
      remove_modal_spinner()
    })

    observeEvent(input$trans_prob, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$trans_prob(data$q_matrix, data$ir_matrix, time_pts, attribute_names)

      output$trans_prob_output <- renderUI({
        table_list <- lapply(1:dim(result)[3], function(i) { # nolint: seq_linter.
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
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$att_class(data$q_matrix,
                               data$ir_matrix,
                               time_pts,
                               attribute_names)

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
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$most_likely_trans(data$q_matrix,
                                       data$ir_matrix,
                                       time_pts,
                                       attribute_names)

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
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$trans_pos(data$q_matrix,
                               data$ir_matrix,
                               time_pts,
                               attribute_names)

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
      attribute_names <- data$param_specs_data$attribute_names

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
        datatable(result$Global.Fit.Stats,
                  options = list(scrollX = TRUE))
      })

      # Render Item Pairs data frame
      output$item_pairs <- renderDT({
        datatable(result$Item.Pairs,
                  options = list(scrollX = TRUE))
      })

      # Render Gloabl Fit Tests data frame
      output$global_fit_tests <- renderDT({
        datatable(result$Global.Fit.Tests,
                  options = list(scrollX = TRUE))
      })

      # Render Global Fit Stats data frame
      output$global_fit_stats2 <- renderDT({
        datatable(result$Global.Fit.Stats2,
                  options = list(scrollX = TRUE))
      })

      # Render Item RMSEA table
      output$item_rmsea <- renderDT({
        item_rmsea_dt <- tdcm$convert_to_datatable(result$Item.RMSEA)
        datatable(item_rmsea_dt,
                  options = list(scrollX = TRUE))
      })

      # Render Misc data table
      output$misc_table <- renderDT({
        datatable(misc_data,
                  options = list(scrollX = TRUE))
      })

      remove_modal_spinner()
    })

    observeEvent(input$att_corr, {
      show_modal_spinner(spin = "fading-circle")
      time_pts <- data$param_specs_data$num_time_points
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$att_corr(data$q_matrix,
                              data$ir_matrix,
                              time_pts,
                              attribute_names)
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
      attribute_names <- data$param_specs_data$attribute_names

      result <- tdcm$reliability(data$q_matrix,
                                 data$ir_matrix,
                                 time_pts,
                                 attribute_names)
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