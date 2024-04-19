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

    print(sheet_name)

    if (!file.exists(file)) {
      write.xlsx(data_table, file = file, sheetName = sheet_name, row.names = TRUE)
    } else {
      write.xlsx(data_table,
                 file = file,
                 sheetName = sheet_name,
                 append = TRUE,
                 row.names = FALSE)
    }
  }
}
