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
    radioButtons("itemParameter",
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

    selectInput("dcmEstimate",
                "DCM to estimate:",
                choices = c("full LCDM",
                            "LDCM1",
                            "LDCM2",
                            "DINA",
                            "Different on each item"),
                selected = "full LCDM"),

    # Dynamic rendering of dropdown menus
    uiOutput("itemDropdowns"),


    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observe({
      # Check if input$dcmEstimate is not NULL and not empty
      if (!is.null(input$dcmEstimate) && input$dcmEstimate != "") {
        console.log("checking")
        if (input$dcmEstimate == "Different on each item") {
          console.log("hit condition")
          output$itemDropdowns <- renderUI({
            selectInput("itemDropdown",
                        "Select an item:",
                        choices = c("Item 1", "Item 2", "Item 3"),
                        selected = NULL)
          })
        } else {
          output$itemDropdowns <- renderUI(NULL)
        }
      }
    })


    ui_components$nb_server("nextButton", "/")
  })
}