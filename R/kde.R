kde <- function(type_kernel = "n", vec_data, y = NULL, bw = PBbw(type_kernel = "n", vec_data, 2)) {
  if (is.null(y)) {
    y <- seq(min(vec_data), max(vec_data), length.out = 100)
  }
  aux <- outer(y, vec_data, "-")
  aux <- kernel_function_distribution(type_kernel, aux / bw)
  result <- apply(aux, 1, mean)
  list(Estimated_values = result, grid = y, bw = bw)
}
