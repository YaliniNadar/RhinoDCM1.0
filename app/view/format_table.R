box::use(
  DT[formatRound, datatable],
)

#' @export
get_formatted_table <- function(table, columns, digits = 3) {
  formatted_table <- formatRound(
    table,
    columns = columns,
    digits = digits
  )

  print("HELOOOOOOOO")
  print(typeof(datatable(formatted_table)))
  return(datatable(formatted_table))
}

#' @export
format_pagination <- function() {
  jquery_code <- "function(settings, json) {$(this.api().table().container()).find('.dataTables_paginate').css({'background-color': '#202020', 'color': '#fff'});}" # nolint
  return(jquery_code)
}
