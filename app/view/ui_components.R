box::use(
  shiny[navbarPage, tabPanel, NS, actionButton, observeEvent, moduleServer],
  shiny.router[change_page],
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
nb_server <- function(id, route) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$nextButton, {
      change_page(route)
    })
  })
}

#' @export
format_pagination <- function() {
  jquery_code <- "function(settings, json) {$(this.api().table().container()).find('.dataTables_paginate').css({'background-color': '#202020', 'color': '#fff'});}" # nolint
  return(jquery_code)
}


# Function to create a download handler for a CSV file
#' @export
create_download_handler <- function(file_name, data_function) {
  downloadHandler(
    filename = function() {
      file_name
    },
    content = function(file) {
      write.csv(data_function(), file)
    }
  )
}
