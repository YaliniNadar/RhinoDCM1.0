box::use(
  xlsx[
    write.xlsx,
  ],
  openxlsx [
    write.xlsx
  ]
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

#' @export
write_2 <- function(dataset_list, file) {
  data_frames <- lapply(dataset_list, as.data.frame)
  # Assuming dataset_list is your list of data tables
  write.xlsx(data_frames, file = file, rowNames = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
}
