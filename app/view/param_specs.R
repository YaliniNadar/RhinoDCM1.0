box::use(
  shiny[moduleServer,
        NS,
        fluidPage,
        h2,
        p,
        br,
        numericInput,
        textInput,
        radioButtons,
        actionButton,
        observeEvent,
        conditionalPanel,
        uiOutput,
        renderUI,
        observe],
)

#' @export
param_specs_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Parameter Specifications"),
    br(),

    # Input 1: Number of time points
    numericInput(ns("num_time_points"), "Enter number of time points: ", value = 1, min = 1),

    # Input 2: Number of attributes measured
    numericInput(ns("num_attributes"), "Enter number of attributes measured: ", value = 1, min = 1),

    # Input 3: Attribute names separated by commas
    textInput(ns("attribute_names"), "Enter attribute names separated by commas: "),

    # Input 4: Q-Matrix for each time point
    radioButtons(ns("q_matrix_choice"), "Is there a different Q-Matrix for each time point?",
                 choices = c("Yes", "No"), selected = "No"),

    conditionalPanel(
      condition = "input.q_matrix_choice == 'No'",
      numericInput(ns("num_items_single_time_point"),
                   "Enter number of items at a single time point: ",
                   value = 1, min = 1)
    ),

    conditionalPanel(
      condition = "input.q_matrix_choice == 'Yes'",
      textInput(ns("num_items_each_time_point"),
                "Enter number of items for each time point separated by commas (no spaces): ")
    ),

    uiOutput(ns("conditional_num_items"))

  )
}

#' @export
param_specs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Render UI conditionally based on q_matrix_choice
    output$conditional_num_items <- renderUI({
      if (input$q_matrix_choice == "No") {
        numericInput(session$ns("num_items_single_time_point"),
                     "Enter number of items at a single time point: ",
                     value = 1, min = 1)
      } else {
        textInput(session$ns("num_items_each_time_point"),
                  "Enter number of items for each time point separated by commas (no spaces): ")
      }
    })

  })
}
