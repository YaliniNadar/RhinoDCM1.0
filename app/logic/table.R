box::use(
  xlsx[
    write.xlsx,
  ],
)

#' @export
write_multiple_sheets <- function(data_with_names, file) {
  for (entry in data_with_names) {
    sheet_name <- entry$name
    data_table <- entry$data

    if (!file.exists(file)) {
      write.xlsx(data_table, file = file, sheetName = sheet_name, row.names = FALSE)
    } else {
      row_names <- if (sheet_name == "Item RMSEA") TRUE else FALSE
      write.xlsx(data_table,
                 file = file,
                 sheetName = sheet_name,
                 append = TRUE,
                 row.names = row_names)
    }
  }
}
