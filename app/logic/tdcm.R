box::use(
  TDCM[
    tdcm,
    data.tdcm01,
    tdcm.summary
  ],
  utils[
    data,
    str,
  ]
)

fit_model <- function(data, q_matrix, num_time_points) {
  model <- tdcm(data, q_matrix, num.time.points = 2)
  return(model)
}

#' @export
tdcm_test <- function(q_matrix, ir_matrix) {
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
    print(item_parameters)

    return(item_parameters)
  }


  # print(q_matrix)
  # print(ir_matrix)
}