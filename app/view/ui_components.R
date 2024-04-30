box::use(
  shiny[
    navbarPage,
    tabPanel,
    NS,
    actionButton,
    observeEvent,
    moduleServer,
    HTML,
    showModal,
    modalDialog,
    modalButton,
    tagList
  ],
  shiny.router[change_page]
)

#' @export
navbar_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = "TDCMApp",
    id = ns("navbar"),
  )
}

#' @export
next_button <- function(id, enabled = TRUE) {
  ns <- NS(id)

  if (enabled) {
    actionButton(
      ns("nextButton"),
      "Next",
      class = "btn-primary",
      style = "float: right; margin-right: 10px;"
    )
  } else {
    tags$button(
      "Next",
      id = ns("nextButton"),
      class = "btn btn-primary disabled",
      style = "float: right; margin-right: 10px;",
      disabled = "disabled"
    )
  }
}

#' @export
back_button <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("backButton"),
    "Back",
    class = "btn-primary",
    style = "float: right; margin-right: 5px; margin-left: 5px;",
    onclick = "App.goBack();"
  )
}

#' @export
reset_button <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("resetBtn"),
    "Back",
    class = "btn-primary",
    style = "float: right; margin-right: 5px; margin-left: 5px;",
  )
}

#' @export
rb_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$resetBtn, {
      print("reset button is clicked")
      # Reset all variables
      # Add more reset actions for other variables as needed
      showModal(modalDialog(
        title = "Confirm Navigation",
        HTML("Are you sure you want to leave this page?<br/><br/>
         This action will erase all entered data requiring you to start over.<br/>
         Make sure to download any file(s) you need before leaving."),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("confirmLeave"), "Yes, Leave"),
          modalButton("No, Stay"),
        )
      ))
    })
    observeEvent(input$confirmLeave, {
      change_page("/")
      session$reload()
    })
  })
}

#' @export
nb_server <- function(id, route) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$nextButton, {
      change_page(route)
    })
  })
}
