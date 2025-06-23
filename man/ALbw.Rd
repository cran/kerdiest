\name{ALbw}
\alias{ALbw}
\title{Compute the Plug-in Bandwidth of Altman and Leger}
\description{The bandwidth parameter for the distribution function kernel
  estimator is calculated, using the plug-in method of Altman and Leger (1995).
  Four possible kernel functions can be used for the kernel estimator: "e"
  Epanechnikov,	"n" Normal, "b" Biweight and "t" Triweight.
}
\usage{ALbw(type_kernel = "n", vec_data)
}
\arguments{
  \item{type_kernel}{The kernel function. You can use four types:
  "e" Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The normal kernel is used by default.
  }
  \item{vec_data}{The data sample.
  }
}
\value{A numeric value for the bandwidth parameter.
}
\details{Altman and Leger (1995) recommend the use of the Epanechnikov kernel,
  because in this case the rate of convergence for the kernel derivative
  estimator is improved. For the sake of uniformity along the package, the
  gaussian kernel is used by default, but the user can obviously choose the
  Epanechnikov function.
}
\references{
  Altman, N. and Leger, C. (1995), "Bandwidth selection for kernel distribution
  function estimation", Journal of Statistical Planning and Inference, 46,
  195-214.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\author{Graciela Estévez Pérez and Alejandro Quintela del Río
}
\examples{
## Compute the plug-in bandwidth for a sample of 100 random N(0,1) data
x <- rnorm(100, 0, 1)
h_AL <- ALbw(type_kernel = "e", vec_data = x)
h_AL
\donttest{
## A quick plot of a distribution function estimate
x <- rnorm(1000)
h_AL <- ALbw(vec_data = x)
F_AL <- kde(vec_data = x, bw = h_AL)
plot(F_AL$grid, F_AL$Estimated_values, type = "l")
## Plotting the distribution function estimate, controling the grid points
## and the kernel function
ss <- quantile(x, c(0.05, 0.95))
## Number of points to be used in the representation of estimated distribution
## function
n_pts <- 100
y <- seq(ss[1], ss[2], length.out = n_pts)
F_AL <- kde(type_kernel = "e", x, y, h_AL)$Estimated_values
## Plot of the theoretical and estimated distribution functions
plot(y, F_AL, type = "l", lty = 2)
lines(y, pnorm(y), type = "l", lty = 1)
legend(-1, 0.8, c("Real", "Nonparametric"), lty = 1:2)
}
}
\keyword{smoothing}
\keyword{nonparametric}
