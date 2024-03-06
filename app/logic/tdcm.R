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

fit_model <- function(data, q_matrix, num_time_points) {
  model <- tdcm(data, q_matrix, num.time.points = 2)
  return(model)
}

fit_and_summarize <- function(q_matrix, ir_matrix, num_time_points) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model <- fit_model(ir_matrix, q_matrix, num_time_points = 2)
    results <- tdcm.summary(model, num.time.points = 2)
    return(results)
  }
}

#' @export
item_parameters <- function(q_matrix, ir_matrix) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    resuls <- fit_and_summarize(q_matrix, ir_matrix, num_time_points)
    item_parameters <- results$item.parameters

    # Debugging output
    print("Dimensions of item_parameters:")
    print(dim(item_parameters))
    print("Structure of item_parameters:")
    print(str(item_parameters))
    print("-------")
    print(item_parameters)

    return(item_parameters)
  }
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
