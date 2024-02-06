box::use(
  shiny[moduleServer,
        NS,
        fluidPage,
        h2,
        p,
        br,
        tags,
        HTML,
        numericInput,
        textInput,
        radioButtons,
        actionButton,
        observeEvent,
        uiOutput,
        renderUI,
        observe],
  shinyjs[useShinyjs, runjs],
)

box::use(
  app/view[ui_components],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    useShinyjs(),
    h2("Parameter Specifications"),
    br(),

    # Input 1: Number of time points
    numericInput(ns("num_time_points"), "Enter number of time points: ", value = 1, min = 1),

    # Input 2: Number of attributes measured
    numericInput(ns("num_attributes"), "Enter number of attributes measured: ", value = 1, min = 1),

    # Input 3: Attribute names separated by commas
    textInput(ns("attribute_names"), "Enter attribute names separated by commas: "),

    # Input 4: Q-Matrix for each time point
    radioButtons(ns("q_matrix_choice"), "Is there a different Q-Matrix for each time point?",
                 choices = c("Yes", "No"), selected = NULL),

    uiOutput(ns("conditional_num_items")),

    tags$script(
      '
      // Beforeunload event to store values in localStorage before refresh
      window.addEventListener("beforeunload", function() {
        console.log("Beforeunload event triggered");
        var numTimePoints = $("#num_time_points").val();
        var numAttributes = $("#num_attributes").val();
        var attributeNames = $("#attribute_names").val();
        var qMatrixChoice = $("input[name=q_matrix_choice]:checked").val();

        // Create an object with the values
        var valuesToSave = {
          num_time_points: num_time_points,
          num_attributes: num_attributes,
          attribute_names: attribute_names,
          q_matrix_choice: q_matrix_choice
        };

        // Convert the object to a JSON string
        var valuesJSON = JSON.stringify(valuesToSave);

        // Save the JSON string to local storage
        localStorage.setItem("saved_values", valuesJSON);
      });

      $(document).ready(function() {
        console.log("Getting values from localStorage on document ready");
        console.log("num_time_points:", localStorage.getItem("num_time_points"));
        console.log("num_attributes:", localStorage.getItem("num_attributes"));
        console.log("attribute_names:", localStorage.getItem("attribute_names"));

        // Read data from local storage
        var numTimePoints = localStorage.getItem("num_time_points");
        var numAttributes = localStorage.getItem("num_attributes");
        var attributeNames = localStorage.getItem("attribute_names");
        var qMatrixChoice = localStorage.getItem("q_matrix_choice");

        // Update UI elements with retrieved data
        $("#num_time_points").val(numTimePoints);
        console.log($("#num_time_points").val())
        $("#num_attributes").val(numAttributes);
        $("#attribute_names").val(attributeNames);

        if (qMatrixChoice !== null) {
          $("input[name=q_matrix_choice][value=" + qMatrixChoice + "]").prop("checked", true);
        }
        
      });
      '
    ),

    ui_components$next_button(ns("nextButton")),
    ui_components$back_button(ns("backButton")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Render UI conditionally based on q_matrix_choice
    output$conditional_num_items <- renderUI({
      if (input$q_matrix_choice == "No") {
        numericInput(session$ns("num_items_single_time_point"),
                     "Enter number of items at a single time point: ",
                     value = 1, min = 1)
      } else {
        textInput(session$ns("num_items_each_time_point"),
                  "Enter number of items for each time point separated by commas (no spaces): ")
      }
    })

    # Save data to local storage
    observe({
      values_to_save <- list(
        num_time_points = input$num_time_points,
        num_attributes = input$num_attributes,
        attribute_names = input$attribute_names,
        q_matrix_choice = input$q_matrix_choice
      )
      values_json <- jsonlite::toJSON(values_to_save, auto_unbox = TRUE)
      runjs(paste("App.saveToLocalStorage(", values_json, ");"))
    })

    ui_components$nb_server("nextButton", "q_matrix")

  })
}
