box::use(
  shinyjs[runjs],
)

performIndexedDBRead <- function(db_name, prefix, fields) {
  # Convert fields to a string
  fields_string <- paste0("['", paste(fields, collapse = "', '"), "']")

  # Construct the JavaScript function call string
  js_function_call <- sprintf("App.readIndexedDBAndSave('%s', %s, '%s')", db_name, fields_string, prefix)

  # Execute the JavaScript function in the Shiny application
  runjs(js_function_call)
}

