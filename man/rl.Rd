\name{rl}
\alias{rl}
\title{
Return level estimation
}
\description{
The T-return level  is
defined as the value of the observed variable that can be
expected to be once exceeded during a T-period of
time. This is computed as the quantile of the distribution, corresponding 
to the value    \eqn{F^{-1}(1-\frac{1}{T})}. 
}
\usage{
rl(type_kernel = "n", vec_data, T, bw = PBbw(type_kernel = "n", 
vec_data, 2))
}
\arguments{
   \item{type_kernel}{The kernel function used. You can use 
  four types:   "e" Epanechnikov,	"n" Normal,  "b" Biweight and
 "t" Triweight.  The Normal kernel is used by default.
}
   \item{vec_data}{
The data sample (earthquake magnitudes, flow levels, wind speeds... ).
}
  \item{T}{             
 A particular value of time, or an array of time values.
 }
  \item{bw}{
The bandwidth parameter. The plug-in method of Polansky and Baker (2000) is 
used by default.
}
}
\details{
In several scientific fields results of interest to estimate quantiles
corresponding to a probability of exceedance. For example, in hydrology, 
the T-return level  \eqn{x_T} is defined as the value of the observed flow
that can be expected to be once exceeded during a T-period of time; that is, 
the quantile
 \deqn{    x_T=F^{-1}(1-\frac{1}{T}).      }
We can estimate it directly by
  \deqn{    \hat{x}_T=F_h^{-1}(1-\frac{1}{T}).      }
  See, for instance, Quintela del Rio (2011), for an application to data of
Salt River near Roosevelt, AZ, USA.
}
\value{
A single value or an array for the estimated quantiles.
}
\references{
 Quintela-del-Rio, A. (2011)  On bandwidth selection for nonparametric estimation
  in flood frequency analysis. \emph{Hydrological Processes}  \bold{25},
  pp. 671--678.

Quintela-del-Rio, A. and Estevez-Perez, G. (2012) 
Nonparametric Kernel Distribution Function Estimation with kerdiest: 
An R Package for Bandwidth Choice and Applications, 
\emph{Journal of Statistical Software} \bold{50(8)}, pp. 1-21.
 URL http://www.jstatsoft.org/v50/i08/.
}
\author{ Graciela Estevez Perez \email{graci@udc.es} and Alejandro Quintela del Rio 
\email{aquintela@udc.es}
}

\examples{
data(saltriver)
peak<-saltriver$peakflow
\dontrun{
year<-saltriver$year
plot(year,peak, type="l",ylab="Annual peak flow")
 }
# Calculating the return values for a period from 2 to 100 years
 T<-seq(2,100, length.out=100)
ret.lev<-rl(vec_data=peak, T=T)
\dontrun{
plot(T, ret.lev, type="l", xlab="years", ylab="Flow (cumecs)", 
main="Return level Plot")
 }
 }
\keyword{ smooth }
\keyword{ nonparametric}
