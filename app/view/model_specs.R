box::use(
  shiny[
    NS,
    fluidPage,
    br,
    h2,
    p,
    selectInput,
    radioButtons,
    textInput,
    moduleServer,
    observe,
    renderUI,
    uiOutput
  ],
)

box::use(
  app / view[ui_components, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Model Specifications"),
    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
    br(),
    p("The default setting is Invariance = True and full DCM.",
      style = "font-size: 14px; font-weight: bold;"
    ),
    radioButtons("itemParameter",
      "Item parameter assumed:",
      choices = c("Yes", "No"),
      selected = "Yes"
    ),
    selectInput("dcmEstimate",
      "DCM to estimate:",
      choices = c(
        "full LCDM",
        "LDCM1",
        "LDCM2",
        "DINA",
        "Different on each item"
      ),
      selected = "full LCDM"
    ),

    # Dynamic rendering of dropdown menus
    uiOutput(ns("dynamicDropdowns")),

    # ui_components$next_button(ns("nextButton")),
    # ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactively render dropdowns
    # output$dynamicDropdowns <- renderUI({
    #   if (input$dcmEstimate == "Different on each item"){
    #     num_items <- 3
    #     dropdowns <- lapply(1:num_items, function(i) {
    #       selectInput(paste0("item", i),
    #                   label = paste("Item", i),
    #                   choices = c("LDCM1", "LDCM2", "DINA"),
    #                   selected = NULL)  # Set the default selected value if needed
    #     })
    #     do.call(tagList, dropdowns)
    #   }
    # })

    ui_components$nb_server("nextButton", "/")
  })
}
