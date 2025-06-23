\name{rl}
\alias{rl}
\title{Return Level Estimation
}
\description{The T-return level is defined as the value of the observed
  variable that can be expected to be once exceeded during a T-period of
  time. This is computed as the quantile of the distribution corresponding
  to the value \eqn{F^{-1}(1-\frac{1}{T})}.
}
\usage{rl(type_kernel = "n", vec_data, time,
          bw = PBbw(type_kernel = "n", vec_data, 2))
}
\arguments{
  \item{type_kernel}{The kernel function used. You can use four types:
  "e" Epanechnikov, "n" Normal, "b" Biweight and "t" Triweight.
  The normal kernel is used by default.
  }
  \item{vec_data}{The data sample (earthquake magnitudes, flow levels, wind
  speeds...).
  }
  \item{time}{A time or a vector of times for T.
  }
  \item{bw}{The bandwidth parameter. By default, he plug-in method of Polansky
  and Baker (2000) is used.
  }
}
\details{In several scientific fields, it is of interest to estimate quantiles
  corresponding to a probability of exceedance. E.g., in hydrology, the
  T-return level \eqn{x_T} is defined as the value of the observed flow
  that can be expected to be once exceeded during a T-period of time; i.e.,
  the quantile
  \deqn{x_T=F^{-1}(1-\frac{1}{T}).}
  It can be directly estimated by
  \deqn{\hat{x}_T=F_h^{-1}(1-\frac{1}{T}).}
  See, e.g., Quintela-del-Rio (2011), for an application to data of Salt River
  near Roosevelt, AZ, USA.
}
\value{A single value or an array for the estimated quantiles.
}
\references{
  Quintela-del-Rio, A. (2011), "On bandwidth selection for nonparametric
  estimation in flood frequency analysis". Hydrological Processes 25, 671-678.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\author{Graciela Estévez Pérez and Alejandro Quintela del Río
}
\examples{
\donttest{
data(saltriver)
peak <- saltriver$peakflow
year <- saltriver$year
plot(year, peak, type = "l", ylab = "Annual peak flow")
## Calculating the return values for a period from 2 to 100 years
times <- seq(2,100, length.out = 100)
ret.lev <- rl(vec_data = peak, time = times)
plot(times, ret.lev, type = "l", xlab = "Years", ylab = "Flow (cumecs)",
main = "Return level Plot")
}
}
\keyword{smoothing}
\keyword{nonparametric}
