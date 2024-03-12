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
  ],
  memoise[
    memoise
  ]
)

fit_and_summarize <- memoise(function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model <- tdcm(ir_matrix, q_matrix, num.time.points = time_pts)
    results <- tdcm.summary(model, num.time.points = time_pts)
    return(results)
  }
})

#' @export
item_parameters <- function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
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
growth <- function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
    growth <- results$growth
    print(growth)
    return(growth)
  }
}

#' @export
visualize <- function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
    plot <- tdcm.plot(results)
    return(plot)
  }
}

#' @export
trans_prob <- function(q_matrix, ir_matrix, time_pts) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
  probs <- results$transition.probabilities
  return(probs)
}
