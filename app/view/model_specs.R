box::use(
  shiny[
    NS,
    fluidPage,
    tags,
    br,
    h2,
    p,
    selectInput,
    radioButtons,
    textInput,
    observeEvent,
    reactiveValues,
    moduleServer,
    conditionalPanel,
    observe,
    renderUI,
    uiOutput
  ],
  DT[DTOutput, renderDT, datatable, JS],
)

box::use(
  app/view[ui_components, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Model Specifications"),
    br(),
    p("The default setting is Invariance = True and full DCM.",
      style = "font-size: 14px; font-weight: bold;"
    ),
    radioButtons(ns("itemParameter"),
      "Item parameter assumed:",
      choices = c("Yes", "No"),
      selected = "Yes"
    ),
    tags$ul(
      tags$li("GDINA: the default DCM, implemented with a logit link to estimate the LCDM"),
      tags$li("ACDM:  estimate the LCDM with only main effects"),
      tags$li("DINA: estimate the DINA model"),
      tags$li("GDINA1: estimate the LCDM with only main effects, equivalent to ACDM"),
      tags$li("GDINA2: estimate the LCDM with up to two-way interaction effects")
    ),
    selectInput(ns("dcmEstimate"),
      "DCM to estimate:",
      choices = c(
        "full LCDM",
        "LDCM1",
        "LDCM2",
        "DINA",
        "ACDM",
        "GDINA1",
        "GDINA2",
        "Different on each item"
      ),
      selected = "full LCDM"
    ),
    DTOutput(ns("itemRadioMatrix")),
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dcmEstimate, {
      # Check if input$dcmEstimate is not NULL and not empty
      if (!is.null(input$dcmEstimate) && input$dcmEstimate != "") {
        if (input$dcmEstimate != "Different on each item") {
          output$itemRadioMatrix <- renderDT(NULL)
        }
      }
    })

    observe({
      # Check if input$dcmEstimate is not NULL and not empty
      if (!is.null(input$dcmEstimate) && input$dcmEstimate != "") {
        num_items <- data$param_specs_data$num_items
        if (input$dcmEstimate == "Different on each item") {
          choices <- c(
            "full LCDM",
            "LDCM1",
            "LDCM2",
            "DINA",
            "ACDM",
            "GDINA1",
            "GDINA2"
          )
          # Generate a matrix of radio buttons
          radio_matrix <- matrix(
            NA,
            nrow = num_items, ncol = length(choices),
            dimnames = list(paste0("Item ", 1:num_items), choices)
          )

          # Fill the matrix with radio buttons
          for (i in seq_len(nrow(radio_matrix))) {
            for (j in seq_len(ncol(radio_matrix))) {
              radio_matrix[i, j] <- sprintf(
                '<input type="radio" name="%s" value="%s"/>',
                rownames(radio_matrix)[i], colnames(radio_matrix)[j]
              )
            }
          }

          # Convert radio_matrix to a data frame
          radio_df <- as.data.frame(radio_matrix, stringsAsFactors = FALSE)

          # Render the radio matrix using renderDT
          output$itemRadioMatrix <- renderDT({
            datatable(
              radio_df,
              escape = FALSE, # Allow HTML
              selection = "none",
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              callback = JS("
                table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());")
            )
          })
        }
      }
    })

    # Saving the itemParameter, dcmEstimate
    observe({
      # Save the value of itemParameter
      data$model_specs_data$itemParameter <- input$itemParameter
      data$model_specs_data$dcmEstimate <- input$dcmEstimate
    })

    ui_components$nb_server("nextButton", "review")
  })
}
