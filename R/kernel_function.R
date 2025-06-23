kernel_function <- function(type_kernel, u) {
  if (type_kernel == "e") {
    result <- u
    logic0 <- (u <= -1 | u >= 1)
    logic1 <- (u > -1 & u < 1)
    result[logic0] <- 0
    Uval <- result[logic1]
    result[logic1] <- 0.75 * (1 - (Uval^2))
    result
  } else {
    if (type_kernel == "n") {
      result <- dnorm(u)
      result
    } else {
      if (type_kernel == "b") {
        result <- u
        logic0 <- (u <= -1 | u >= 1)
        logic1 <- (u > -1 & u < 1)
        result[logic0] <- 0
        Uval <- result[logic1]
        result[logic1] <- (15 / 16) * ((1 - (Uval^2)))^2
        result
      } else {
        if (type_kernel == "t") {
          result <- u
          logic0 <- (u <= -1 | u >= 1)
          logic1 <- (u > -1 & u < 1)
          result[logic0] <- 0
          Uval <- result[logic1]
          result[logic1] <- (35 / 32) * ((1 - (Uval^2)))^3
          result
        }
      }
    }
  }
}
