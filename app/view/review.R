box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    h4,
    fluidRow,
    column,
    wellPanel,
    observeEvent,
    moduleServer,
    observe,
    renderText,
    textOutput,
  ],
)

box::use(
  app/view[ui_components],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Review"),
    br(),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),

    # Main content blocks
    fluidRow(
      column(3, wellPanel(
        h4("Parameter Specs"),
        textOutput(ns("param_specs"))
      )),
      column(3, wellPanel(
        h4("Q-Matrix"),
        textOutput(ns("q_matrix"))
      )),
      column(3, wellPanel(
        h4("IR Matrix"),
        textOutput(ns("ir_matrix"))
      )),
      column(3, wellPanel(
        h4("Model Specs"),
        textOutput(ns("model_specs"))
      ))
    ),

    # Matrix previews
    fluidRow(
      column(6, wellPanel(
        h4("Q-Matrix Preview"),
        # Placeholder for Q-Matrix preview
      )),
      column(6, wellPanel(
        h4("IR Matrix Preview"),
        # Placeholder for IR Matrix preview
      ))
    ),

    # Navigation buttons
    fluidRow(
      column(6, ui_components$back_button(ns("backButton"))),
      column(6, ui_components$next_button(ns("nextButton")))
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to generate the text for parameter specifications
    generate_param_specs <- function() {
      num_time_points <- data$param_specs_data$num_time_points
      num_attributes <- data$param_specs_data$num_attributes
      attribute_names <- data$param_specs_data$attribute_names
      q_matrix_choice <- data$param_specs_data$q_matrix_choice
      num_items_single_time_point <- data$param_specs_data$num_items

      param_specs <- paste(
        "Number of time points:", ifelse(is.null(num_time_points), "N/A", num_time_points),
        "Number of attributes measured:", ifelse(is.null(num_attributes), "N/A", num_attributes),
        "Attribute Names:", ifelse(is.null(attribute_names), "N/A", attribute_names),
        "Is there a different Q-Matrix for each time point:",
        ifelse(is.null(q_matrix_choice), "N/A", q_matrix_choice),
        "Number of items at a single time point:",
        ifelse(is.null(num_items_single_time_point), "N/A", num_items_single_time_point),
        sep = "\n"
      )

      return(param_specs)
    }

    # Define the text to be displayed in each section
    output$param_specs <- renderText({
      generate_param_specs()
    })

    output$q_matrix <- renderText("Q-Matrix content will be displayed here.")
    output$ir_matrix <- renderText("IR Matrix content will be displayed here.")
    output$model_specs <- renderText({
      paste("Invariance: Yes",
            "DCM to estimate: Different on each item",
            "Item 1: full DCM",
            "Item 2: TDCM2",
            "Item 3: TDCM1",
            sep = "\n")
    })

    ui_components$nb_server("nextButton", "tdcm_test")
  })
}