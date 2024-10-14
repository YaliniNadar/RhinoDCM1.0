box::use(
  shiny[
    moduleServer,
    NS,
    fluidPage,
    titlePanel,
    mainPanel,
    div,
    p,
    fluidRow,
    column,
    br,
    actionButton,
    observeEvent,
    h1,
    reactiveValues,
    icon,
    tags
  ],
  shinyBS[
    bsTooltip,
    bsModal,
    toggleModal
  ],
  shiny.router[change_page],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("TDCMApp", align = "center"),
    br(),
    p(
      "TDCMApp provides a user-friendly interface to help you gain valuable insights from your test
      data.",
      align = "center"
    ),
    br(),
    mainPanel(
      fluidRow(
        column(
          6,
          p(
            "Choose Single Group if you have only one group of students who took the test. Ideal for
            assessing the overall performance of a general student population."
          ),
          br(),
          # Information icon with tooltip
          # DO NOT PUT `'` in title, if so the title wont pop up
          # also you cannot shorten the titel or else same problem ^
          icon("info-circle", id = ns("single_info"), style = "cursor: pointer; color: #007bff;"),
          bsTooltip(
            id = ns("single_info"),
            title = "Single Group analysis is suitable when you are evaluating the performance of a single, homogeneous group of students without any subdivisions.",
            placement = "right",
            trigger = "hover"
          ),

          br(),
          actionButton(
            inputId = ns("go_to_param_specs"),
            label = "Single Group",
            class = "btn-primary btn-md"
          )
        ),
        column(
          6,
          p(
            "Choose Multi Group when evaluating multiple groups of students who took the test.
            Especially useful for scenarios like educational interventions, where you may have
            distinct control and treatment groups."
          ),
          # Information icon with tooltip
          icon("info-circle", id = ns("multi_info"), style = "cursor: pointer; color: #6c757d;"),
          bsTooltip(
            id = ns("multi_info"),
            title = "Multi Group analysis allows you to compare different subgroups within your data, such as control vs. treatment groups, to identify differential effects.",
            # If you try to shorten this title it wont show at all
            placement = "right",
            trigger = "hover"
          ),
          br(),
          actionButton(
            inputId = ns("button2"),
            label = "Multi Group",
            class = "btn-primary btn-md",
            disabled = "disabled"
          ),
          br(),
          p(
            "Multi Group functionality will be available in a future update.",
            style = "color: grey; margin-top: 10px;"
          )
        )
      ),
      br(),
      # helper button
      fluidRow(
        column(
          12,
          actionButton(
            inputId = ns("help_button"),
            label = "Need Help Choosing?",
            class = "btn-secondary",
            icon = icon("question-circle")
          )
        )
      )
    ),

    # Define modals
    bsModal(
      id = "coming_soon_modal",
      title = "Coming Soon",
      trigger = ns("button2"),  # Triggered by "Multi Group" button
      size = "small",
      "The Multi Group feature is under development and will be available in a future update."
    ),

    bsModal(
      id = "help_modal",
      title = "Help: Choosing Between Single and Multi Group",
      trigger = ns("help_button"),  # Triggered by "Need Help Choosing?" button
      size = "large",
      p(
        "Use the following guidelines to select the appropriate option:",
        tags$ul(
          tags$li("Select Single Group if you are analyzing data from a single, homogeneous group of
          students without any subdivisions."),
          tags$li("Select Multi Group (when available) if you need to compare different subgroups,
          such as control and treatment groups, to assess the impact of specific interventions.")
        )
      )
    )
  )
}

#' @export
server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_to_param_specs, {
      change_page("param_specs")
    })

    # Handle clicks on the disabled "Multi Group" button
    observeEvent(input$button2, {
      toggleModal(session, "coming_soon_modal", toggle = "open")
    })

    # Show a helper modal when the "Need Help Choosing?" button is clicked
    observeEvent(input$help_button, {
      toggleModal(session, "help_modal", toggle = "open")
    })
  })
}
