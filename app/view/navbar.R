box::use(shiny[navbarPage, tabPanel, NS])

#' @export
ui <- function(id) {
  ns <- NS(id)

  navbarPage(
    title = "DCM",
    id = ns("navbar"),
    tabPanel("Home", value = "home"),
    tabPanel("Page 1", value = "page1"),
    tabPanel("Page 2", value = "page2"),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Your server logic for the navbar can go here if needed
  })
}
