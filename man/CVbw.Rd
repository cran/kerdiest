\name{CVbw}
\alias{CVbw}
\title{Compute the Cross-Validation Bandwidth of Bowman et al (1998)
}
\description{The bandwidth parameter for the distribution function kernel
  estimator is calculated using the modified cross-validation method of Bowman,
  Hall and Prvan (1998). Four possible kernel functions can be used: "e"
  Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The cross-validation function involves an integral term, that is calculated
  using Simpson's rule.
}
\usage{CVbw(type_kernel = "n", vec_data, n_pts = 100, seq_bws = NULL)
}
\arguments{
  \item{type_kernel}{The kernel function. You can use four types:
  "e" Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The normal kernel is used by default.
}
  \item{vec_data}{The data  sample.
}
  \item{n_pts}{The number of points used to approximate, by Simpson's rule,
  the integral term in the cross-validation function. Because this numeric
  method increases the computing time, you can check different numbers,
  depending on your sample size. By default, 100 points are used.
}
  \item{seq_bws}{The sequence of bandwidths in which to compute the
  cross-validation function. By default, the procedure defines a sequence of
  50 points, from the range of the data divided by 200 to the range divided
  by 2.
}
}
\value{A list consisting of:
  \item{seq_bws}{The sequence of bandwidths.
  }
  \item{CV function }{The values of the cross-validation function in the
    bandwidths grid.
  }
  \item{bw}{A numeric value for the cross-validation bandwidth.
  }
}
\references{
  Bowman, A.W., Hall, P. and Prvan,T. (1998), "Cross-validation for the
  smoothing of distribution functions", Biometrika. 85, 799-808.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\author{Graciela Estévez Pérez and Alejandro Quintela del Río
}
\examples{
\donttest{
## Compute the cross-validation bandwidth for a sample of 100 random N(0,1)
## data
x <- rnorm(100, 0, 1)
num_bws <- 50
seq_bws <- seq(((max(x) - min(x))/2)/50, (max(x) - min(x))/2,length = num_bws)
hCV <- CVbw(type_kernel = "e", vec_data = x, n_pts = 200, seq_bws = seq_bws)
hCV
## The CV function is plotted
h_CV <- CVbw(vec_data = x)
h_CV$bw
plot(h_CV$seq_bws, h_CV$CVfunction, type = "l")
## Plotting the distribution function estimate controlling the grid points
## and the kernel function
ss <- quantile(x, c(0.05, 0.95))
## Number of points to be used in the representation of estimated distribution
## function
n_pts <- 100
y <- seq(ss[1], ss[2], length.out = n_pts)
F_CV <- kde(type_kernel = "e", x, y, h_CV$bw)$Estimated_values
## Plot of the theoretical and estimated distribution functions
plot(y, F_CV, type = "l", lty = 2)
lines(y, pnorm(y), type = "l", lty = 1)
legend(-1, 0.8, c("Real", "Nonparametric"), lty = 1:2)
}
}
\keyword{smoothing}
\keyword{nonparametric}
