box::use(
  openxlsx [
    write.xlsx
  ]
)

#' @export
write_multiple_sheets  <- function(dataset_list, file) {
  data_frames <- lapply(dataset_list, as.data.frame)
  # Assuming dataset_list is your list of data tables
  write.xlsx(data_frames, file = file, rowNames = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
}
