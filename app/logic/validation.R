#' @export
pos_int_input <- function(id, label, value = 1, min = 1, max = Inf, input) {
  ns <- NS(id)
  input$add_rule(id, sv_required())
  input$add_rule(id, ~ if (!is.numeric(.)) "Input must be a number")
  input$add_rule(id, ~ if (. != round(.)) "Input must be an integer")
  input$add_rule(id, ~ if (. <= 0) "Input must be positive")
}

#' @export
