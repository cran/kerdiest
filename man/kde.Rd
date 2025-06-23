\name{kde}
\alias{kde}
\title{Kernel Estimation of the Distribution Function
}
\description{Computes the value of the kernel estimator of the distribution
  function, in a single value or in a grid. Four possibilites for the kernel
  function are implemented, and the bandwidth parameter can be directly
  calculated by the plug-in method of Polansky and Baker (2000).
}
\usage{kde(type_kernel = "n", vec_data, y = NULL,
           bw = PBbw(type_kernel = "n", vec_data, 2))
}
\arguments{
  \item{type_kernel}{The kernel function. You can use four types:
  "e" Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The normal kernel is used by default.
  }
  \item{vec_data}{The data sample.
  }
  \item{y}{The single value or the grid vector where the distribution function
 is estimated. By default, a grid of 100 equidistant points from the minimum
 to the maximum of the data sample is selected.
  }
  \item{bw}{The bandwidth used. If it is not provided, the plug-in bandwidth
  of Polansky and Baker (2000) is computed.
  }
}
\value{A list containing:
  \item{Estimated_values}{Vector containing the estimated function in the
  grid values.
  }
  \item{grid}{The used grid.
  }
  \item{bw}{Value of the bandwidth.
  }
}
\references{
  Reiss, R.D. (1981), "Nonparametric estimation of smooth distribution
  functions", Scandinavian Journal of Statistics, 8, 116-119.

  Simonoff, J. (1996), "Smoothing Methods in Statistics", Springer, New York.

  Polansky, A.M. and Baker, E.R. (2000), "Multistage plug-in bandwidth
  selection for kernel distribution function estimates", Journal of Statistical
  Computation and Simulation, 65, 63-80.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\author{Graciela Estévez Pérez and Alejandro Quintela del Río
}
\examples{
\donttest{
## Comparison of three bandwidth selection methods
x <- rnorm(100)
## The bandwidths by cross-validation, plug-in of Altman and Leger
## and plug-in of Polansky and Baker are computed using a normal kernel
## and a standard setting of parameters
h_CV <- CVbw(vec_data = x)$bw
h_CV
## Plug-in of Altman and Leger
h_AL <- ALbw(vec_data = x)
h_AL
## Plug-in of Polansky and Baker
h_PB <- PBbw(vec_data = x)
h_PB
## Plot of the three estimates together with the real distribution
F_CV <- kde(vec_data = x, bw = h_CV)
F_AL <- kde(vec_data = x, bw = h_AL)
F_PB <- kde(vec_data = x, bw = h_PB)
y <- F_CV$grid
Ft <- pnorm(y)
plot(y, Ft, ylab = "Distribution", xlab = "Data", type = "l", lty = 1)
lines(y, F_CV$Estimated_values, type = "l", lty = 2)
lines(y, F_AL$Estimated_values, type = "l", lty = 3)
lines(y, F_PB$Estimated_values, type = "l", lty = 4)
legend(1, 0.4, c("Real", "F_CV", "F_AL", "F_PB"), lty = 1:4)
}
}
\keyword{smoothing}
\keyword{nonparametric}
