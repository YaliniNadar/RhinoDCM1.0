box::use(
  DT[formatRound, datatable, renderDT],
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
      tryCatch(
        {
          png(file)
          print(image_data)  # Assuming image_data is a plot or binary image data
          dev.off()
        },
        error = function(e) {
          # Print error message to console
          cat("Error while generating the image:", conditionMessage(e), "\n")
          # Optionally, you can save the error message to a log file or display it to the user
          # For example, you can use the shinyalert package to display an alert to the user
          shinyalert::showAlert(
            title = "Error",
            text = paste("An error occurred while generating the image:", conditionMessage(e)),
            type = "error"
          )
        }
      )
    }
  )
}
