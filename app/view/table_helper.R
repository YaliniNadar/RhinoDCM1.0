box::use(
  DT[formatRound, datatable],
  shiny[downloadHandler],
  grDevices[
    png,
    dev.off
  ],
  graphics[
    plot
  ]
)

#' @export
format_pagination <- function() {
  jquery_code <- "function(settings, json) {$(this.api().table().container()).find('.dataTables_paginate').css({'background-color': '#202020', 'color': '#fff'});}" # nolint
  return(jquery_code)
}

# Function to create a download handler for an Excel file
#' @export
create_download_handler <- function(data, filename) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      write.xlsx(data, file, sheetName = "Sheet1", row.names = FALSE)
    }
  )
}

# Function to create a download handler for an Image File
#' @export
create_image_download_handler <- function(image_data, filename) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      png(file)
      print(image_data)  # Assuming image_data is a plot or binary image data
      dev.off()
    }
  )
}
