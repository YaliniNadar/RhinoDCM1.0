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
    selectInput("dcmEstimate",
                "DCM to estimate:",
                choices = c("full LCDM",
                            "LDCM1",
                            "LDCM2",
                            "DINA",
                            "Different on each item"),
                selected = "full LCDM"),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Conditional Rendering for Custom Separator

    ui_components$nb_server("nextButton", "/")
  })
}