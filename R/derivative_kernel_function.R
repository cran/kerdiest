derivative_kernel_function <- function(type_kernel, u) {
  if (type_kernel == "e") {
    result <- u
    logic0 <- (u <= -1 | u >= 1)
    logic1 <- (u > -1 & u < 1)
    result[logic0] <- 0
    Uval <- result[logic1]
    result[logic1] <- -1.5 * Uval
    result
  } else {
    if (type_kernel == "n") {
      result <- (1 / (sqrt(2 * pi))) * exp(-(u^2) / 2) * (-u)
      result
    } else {
      if (type_kernel == "b") {
        result <- u
        logic0 <- (u <= -1 | u >= 1)
        logic1 <- (u > -1 & u < 1)
        result[logic0] <- 0
        Uval <- result[logic1]
        result[logic1] <- -(15 / 4) * Uval * (1 - (Uval^2))
        result
      } else {
        if (type_kernel == "t") {
          result <- u
          logic0 <- (u <= -1 | u >= 1)
          logic1 <- (u > -1 & u < 1)
          result[logic0] <- 0
          Uval <- result[logic1]
          result[logic1] <- -(105 / 16) * Uval * ((1 - (Uval^2)))^2
          result
        }
      }
    }
  }
}
