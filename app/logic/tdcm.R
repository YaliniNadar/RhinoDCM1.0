box::use(
  TDCM[
    tdcm,
    data.tdcm01,
    tdcm.summary,
    tdcm.plot
  ],
  utils[
    data,
    str,
  ]
)

process_text_to_df <- function(text) {
  # Check if text is NULL or empty
  if (is.null(text) || length(text) == 0) {
    print("Input is null or empty")
    return(NULL)
  }

  # Split the text into rows
  rows <- strsplit(text, "\n")[[1]]
  
  # Check if rows is empty
  if (length(rows) == 0) {
    print("Length of rows is 0")
    return(NULL)
  }

  # Extract column names
  col_names <- unlist(strsplit(rows[1], "\\s+"))
  
  # Check if column names were extracted
  if (length(col_names) == 0) {
    print("No column names extracted")
    return(NULL)
  }
  # Remove leading and trailing whitespace from column names
  col_names <- trimws(col_names)
  print("Column names:")
  print(col_names)

  # Remove first row (column names)
  rows <- rows[-1]

  # Check if rows is empty after removing column names
  if (length(rows) == 0) {
    print("No data rows after removing column names")
    return(NULL)
  }

  # Split each row into elements
  data <- lapply(rows, function(row) {
    unlist(strsplit(row, "\\s+"))
  })

  # Convert to data frame
  df <- as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE)

  # Set column names
  colnames(df) <- col_names
  print("Data frame after setting column names:")
  print(df)

  # Set row names
  row.names(df) <- paste("Item", 1:nrow(df), sep = "")

  # Replace '--' with NA
  df[df == "--"] <- NA
  print("Final data frame:")
  print(df)

  return(df)
}


fit_model <- function(data, q_matrix, num_time_points) {
  model <- tdcm(data, q_matrix, num.time.points = 2)
  return(model)
}

#' @export
item_parameters <- function(q_matrix, ir_matrix) {
  # data(data.tdcm01, package = "TDCM")
  # data <- data.tdcm01$data
  # q_matrix <- data.tdcm01$q.matrix
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model1 <- fit_model(ir_matrix, q_matrix, num_time_points = 2)
    results1 <- tdcm.summary(model1, num.time.points = 2)
    item_parameters <- results1$item.parameters
    # print(data)
    # print(q_matrix)

    # Debugging output
    print("Dimensions of item_parameters:")
    print(dim(item_parameters))
    print("Structure of item_parameters:")
    print(str(item_parameters))
    print("-------")
    print(item_parameters)

    processed_df <- process_text_to_df(item_parameters)
    print("Structure of processed_df:")
    print(str(processed_df))

    return(item_parameters)
  }


  # print(q_matrix)
  # print(ir_matrix)
}

#' @export
growth <- function(q_matrix, ir_matrix) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model1 <- fit_model(ir_matrix, q_matrix, num_time_points = 2)
    results1 <- tdcm.summary(model1, num.time.points = 2)
    growth <- results1$growth

    print(growth)
    return(growth)
  }
}

#' @export
visualize <- function(q_matrix, ir_matrix) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model1 <- fit_model(ir_matrix, q_matrix, num_time_points = 2)
    results1 <- tdcm.summary(model1, num.time.points = 2)
    plot <- tdcm.plot(results1)
    return(plot)
  }
}
