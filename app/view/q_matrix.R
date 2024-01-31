box::use(
  shiny[
        NS,
        fluidPage,
        br,
        h2,
        fileInput,
        radioButtons,
        textInput,
        checkboxInput,
        conditionalPanel,
        moduleServer],
)

box::use(
  app/view[ui_components, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2("Upload Q-Matrix File"),
    br(),

    # Input: Upload Q-Matrix file
    fileInput(ns("fileQ"), "Choose Q-Matrix File"),

    # Input: Separator type
    radioButtons(ns("separatorType"), "Separator Type:",
                 choices = c("Tab", "Comma", "Custom"), selected = "Comma"),

    # Input: Custom separator
    conditionalPanel(
      condition = "input.separatorType == 'Custom'",
      textInput(ns("customSeparator"), "Enter Custom Separator:")
    ),

    # Input: Additional options
    checkboxInput(ns("excludeHeaders"), "Exclude Header Rows", value = FALSE),
    checkboxInput(ns("excludeIdColumns"), "Exclude ID Columns", value = FALSE),

    ui_components$next_button(ns("nextButton")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}