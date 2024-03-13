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
  ],
  stringr[
    str_squish
  ],
  data.table[
    as.data.table
  ]
)

fit_and_summarize <- memoise(function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    model <- tdcm(ir_matrix, q_matrix, num.time.points = time_pts)
    results <- tdcm.summary(model, num.time.points = time_pts)
    return(results)
  }
})

item_parameters_as_data_table <- function(item_parameters) {

  item_parameters_nrow <- nrow(item_parameters)
  item_parameters_ncol <- ncol(item_parameters)

  item_parameters_rownames <- rownames(item_parameters)
  item_parameters_colnames <- colnames(item_parameters)

  item_parameters_double <- sapply(as.matrix(item_parameters), function(value) {
    value <- str_squish(value)
    if (identical(value, "--")) {
      value <- NA_real_
    } else {
      value <- as.double(value)
    } # if
    return(value)
  })

  names(item_parameters_double) <- NULL

  item_parameters_matrix <- matrix(
    data = item_parameters_double,
    nrow = item_parameters_nrow,
    ncol = item_parameters_ncol,
    dimnames = list(item_parameters_rownames, item_parameters_colnames)
  )

  item_parameters_data_table <- as.data.table(
    item_parameters_matrix,
    keep.rownames = TRUE
  )

  return(item_parameters_data_table)
}

#' @export
item_parameters <- function(q_matrix, ir_matrix, time_pts) {
  if (!is.null(q_matrix) && !is.null(ir_matrix)) {
    results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
    item_parameters <- item_parameters_as_data_table(results$item.parameters)
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

#' @export
att_class <- function(q_matrix, ir_matrix, time_pts) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
  classifications <- results$classifications
  return(classifications)
}

#' @export
most_likely_trans <- function(q_matrix, ir_matrix, time_pts) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
  most_likely_trans <- results$most.likely.transitions
  return(most_likely_trans)
}

#' @export
trans_pos <- function(q_matrix, ir_matrix, time_pts) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
  trans_pos <- results$posterior.probabilities
  return(trans_pos)
}

#' @export
model_fit <- function(q_matrix, ir_matrix, time_pts) {
  results <- fit_and_summarize(q_matrix, ir_matrix, time_pts)
  model_fit <- results$model.fit
  return(model_fit)
}
