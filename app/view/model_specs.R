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
        moduleServer,
        conditionalPanel,
        observe,
        renderUI,
        uiOutput],
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
      style = "font-size: 14px; font-weight: bold;"),
    radioButtons(ns("itemParameter"),
                 "Item parameter assumed:",
                 choices = c("Yes", "No"),
                 selected = "Yes"),

    tags$ul(
      tags$li("GDINA: the default DCM, implemented with a logit link to estimate the LCDM"),
      tags$li("ACDM:  estimate the LCDM with only main effects"),
      tags$li("DINA: estimate the DINA model"),
      tags$li("GDINA1: estimate the LCDM with only main effects, equivalent to ACDM"),
      tags$li("GDINA2: estimate the LCDM with up to two-way interaction effects")
    ),

    selectInput(ns("dcmEstimate"),
                "DCM to estimate:",
                choices = c("full LCDM",
                            "LDCM1",
                            "LDCM2",
                            "DINA",
                            "ACDM",
                            "GDINA1",
                            "GDINA2",
                            "Different on each item"),
                selected = "full LCDM"),

    # Dynamic rendering of dropdown menus
    uiOutput(ns("itemDropdowns")),


    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    num_dropdowns <- 3
    observe({
      # Check if input$dcmEstimate is not NULL and not empty
      if (!is.null(input$dcmEstimate) && input$dcmEstimate != "") {
        if (input$dcmEstimate == "Different on each item") {

          # Generate a list of dropdowns
          dropdown_list <- lapply(1:num_dropdowns, function(i) {
            selectInput(paste0("itemDropdown_", i),
                        label = paste("Select item", i, ":"),
                        choices = c(
                                    "full LCDM",
                                    "LDCM1",
                                    "LDCM2",
                                    "DINA",
                                    "ACDM",
                                    "GDINA1",
                                    "GDINA2" ),
                        selected = NULL)
          })

          # Render the list of dropdowns
          output$itemDropdowns <- renderUI({
            dropdown_list
          })
        } else {
          output$itemDropdowns <- renderUI(NULL)
        }
      }
    })


    ui_components$nb_server("nextButton", "review")
  })
}