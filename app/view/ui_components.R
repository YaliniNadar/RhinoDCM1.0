box::use(
  shiny[
    navbarPage,
    tabPanel,
    NS,
    actionButton,
    observeEvent,
    moduleServer,
    downloadHandler,
    HTML,
    showModal,
    modalDialog,
    modalButton,
    tagList
  ],
  shiny.router[change_page],
  xlsx[
    write.xlsx
  ]
)

#' @export
navbar_ui <- function(id) {
  ns <- NS(id)

  navbarPage(
    title = "TDCMApp",
    id = ns("navbar"),
    # tabPanel("Primary Aggregate Results", value = "primary_aggregate_results.R"),
    # tabPanel("Primary Aggregate Results", value = "primary_individual_results.R"),
    # tabPanel("Secondary Results", value = "secondary_results.R"),
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

# Function to create a download handler for an Excel file
#' @export
create_download_handler <- function(data, filename) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      write.xlsx(data, file, sheetName = "Sheet1", row.names = FALSE)
    }
  )
}
