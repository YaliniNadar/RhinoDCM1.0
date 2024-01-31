box::use(shiny[navbarPage, tabPanel, NS, actionButton, observeEvent, moduleServer],
         shiny.router[change_page], )

#' @export
navbar_ui <- function(id) {
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
next_button <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("nextButton"),
    "Next",
    class = "btn-primary",
    style = "float: right; margin-right: 10px;"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$nextButton, {
      change_page("/")
    })
  })
}
