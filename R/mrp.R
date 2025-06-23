mrp <- function(type_kernel = "n", vec_data, y = NULL, bw = PBbw(type_kernel = "n", vec_data, 2), lambda) {
  if (is.null(y)) {
    y <- seq(min(vec_data), max(vec_data), length.out = 50)
  }
  aux <- outer(y, vec_data, "-")
  aux <- kernel_function_distribution(type_kernel, aux / bw)
  result <- apply(aux, 1, mean)
  mrp_result <- 1 / (lambda * (1 - result))
  list(Estimated_values = mrp_result, grid = y, bw = bw)
}
