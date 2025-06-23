rl <- function(type_kernel = "n", vec_data, time, bw = PBbw(type_kernel = "n", vec_data, 2)) {
  s0 <- min(vec_data)
  s1 <- max(vec_data)
  p <- 1 - 1 / time
  nw <- length(p)
  rl <- 0
  for (i in 1:nw) {
    rl[i] <- dichotomy_fun(type_kernel = "n", vec_data, s0, s1, bw, p[i])
  }
  rl
}
