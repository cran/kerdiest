ef <- function(type_kernel = "n", vec_data, c, bw = PBbw(type_kernel = "n", vec_data, 2), Dmin = 0, Dmax = 15, size_grid = 50, lambda) {
  D <- seq(Dmin, Dmax, length.out = size_grid)
  F_c <- kde(type_kernel, vec_data = vec_data, y = c, bw = bw)$Estimated_values
  exc <- 1 - exp(-lambda * D * (1 - F_c))
  list(Estimated_values = exc, grid = D, bw = bw)
}
