A2_k <- function(type_kernel) {
  if (type_kernel == "e") {
    result <- 1 / 5
  } else {
    if (type_kernel == "n") {
      result <- 1
    } else {
      if (type_kernel == "b") {
        result <- 1 / 7
      } else {
        if (type_kernel == "t")	{
          result <- 1 / 9
        }
        result
      }
    }
  }
}
