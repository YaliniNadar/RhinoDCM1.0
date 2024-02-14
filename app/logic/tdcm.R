box::use(
  TDCM[
    tdcm,
    data.tdcm01,
    tdcm.summary
  ],
  utils[
    data
  ]
)

#' @export
tdcm_test <- function() {
  data(data.tdcm01, package = "TDCM")
  data <- data.tdcm01$data
  q_matrix <- data.tdcm01$q.matrix
  model1 <- tdcm(data, q_matrix, num.time.points = 2)
  results1 <- tdcm.summary(model1, num.time.points = 2)
  item_parameters <- results1$item.parameters
  print(item_parameters)
}