\name{ef}
\alias{ef}
\title{Exceedance Function Estimation
}
\description{Computes the exceedance probability, i.e., the probability that a
  specified value \code{c} (a magnitude of a seismic event, a flow level...)
  will be exceeded in \code{D} time units.
}
\usage{ef(type_kernel = "n", vec_data, c,
          bw = PBbw(type_kernel = "n", vec_data, 2), Dmin = 0, Dmax = 15,
          size_grid = 50, lambda)
}
\arguments{
  \item{type_kernel}{The kernel function. You can use four types:
  "e" Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The Normal kernel is used by default.
  }
  \item{vec_data}{The data sample (earthquake magnitudes, flow levels,
  wind speed...)
  }
  \item{c}{The concrete level in which we want to compute the exceedance
  probability.
  }
  \item{bw}{The bandwidth parameter. The plug-in method of Polansky and
  Baker (2000) is used by default.
  }
  \item{Dmin}{Minimum value for D time units (years, days... ). Default is 0.
  }
  \item{Dmax}{Maximum value for D time units (years, days... ). Default is 15.
  }
  \item{size_grid}{Length of a grid in which we compute the exceedance function.
  By default, 50.
  }
  \item{lambda}{The mean activity rate.
  }
}
\details{The exceedance function is usually calculated assuming that event
  occurrence follows a Poisson process. In this case, the exceedance
  function, i.e., the probability of an specific value \code{c} is calculated
  as
  \deqn{R(c,D) = 1 - exp(-\lambda D(1-F_h(c)).}
  See, e.g., Orlecka-Sikora (2008) or Quintela-del-Rio (2010) for earthquake
  data applications.
}
\value{Returns a list containing:
   \item{Estimated_values}{Vector containing the estimated function.}
   \item{grid}{The used grid.}
   \item{bw}{Value of the  bandwidth.}
}
\references{
  Orlecka-Sikora, B. (2008), "Resampling methods for evaluating the uncertainty
  of the nonparametric magnitude distribution estimation in the probabilistic
  seismic hazard analysis", Tectonophysics 456, 38-51.

  Quintela-del-Rio, A. (2010), "On nonparametric techniques for area-characteristic seismic hazard parameters", Geophysical Journal International, 180, 339-346.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\author{Graciela Estévez Pérez and Alejandro Quintela del Río
}
\examples{
\donttest{
## Working with earthquake data. We use the catalogue of the National
## Geographic Institute (IGN) of Spain and select the data of the Northwest
## of the Iberian Peninsula.
data(nwip)
require(chron)
require(date)
## The data with magnitude greater than 3 are considered
mg <- nwip$magnitude[nwip$magnitude > 3.0]
x1 <- nwip$year
x2 <- nwip$month
x3 <- nwip$day
ys <- paste(x1, x2, x3)
earthquake_date <- as.character(ys)
y1s <- as.date(earthquake_date, order = "ymd")
## Computation of the total number of years
y2s <- as.POSIXct(y1s)
z <- years(y2s)
n.years <- length(levels(z))
## Mean rate of earthquakes per year
lambda <- length(mg)/n.years
## Estimation of the exceedance probability for magnitude = 4
est <- ef(vec_data = mg, c = 4, lambda = lambda)
plot(est$grid, est$Estimated_values, type = "l", xlab = "Years",
  ylab = "Probability of Exceedance")
}
}
\keyword{smoothing}
\keyword{nonparametric}
