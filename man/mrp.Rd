\name{mrp}
\alias{mrp}
\title{
Mean return period estimation
}
\description{
This functions computes an estimate of the time between two values of a concrete
level (size of an earthquake, flow lewel, wind speed... ).
}
\usage{
mrp(type_kernel = "n", vec_data, y = NULL, 
bw = PBbw(type_kernel = "n", vec_data, 2), lambda)
}
\arguments{
 \item{type_kernel}{The kernel function. You can use 
  four types:   "e" Epanechnikov,	"n" Normal,  "b" Biweight and
 "t" Triweight.  The Normal kernel is used by default.
}
   \item{vec_data}{
The data sample (earthquake magnitudes, flow levels, wind speed... ).
}
  \item{y}{
A grid or a singular value where the estimator is computed. By default,
a grid of 50 values between the minimum and the maximum of the data is
computed.
}
  \item{bw}{
The bandwidth parameter. The plug-in method of Polansky and Baker (2000) is 
used by default.
}
 \item{lambda}{
The mean activity rate.
}
}
\details{
The mean return period is usually calculated assuming that the occurrence 
process of the events follows a Poisson one. In this case, the mean return 
period of events of size \code{c} is calculated as
\deqn{   T(c) = \frac{1}{ \lambda (1-F_h(c))}.}
In Orlecka-Sikora (2008) or Quintela del Rio (2010) an application to earthquake
data is made. 
In hydrological applications, if we work with annual maxima data, the parameter of
the Poisson variable is 1 (one maximum per year). The mean return period between 
flow levels of value \code{c} is calculated as
\deqn{   T(c) = \frac{1}{ 1-F_h(c)}.}
See, for instance, Quintela del Rio (2011), for an application to data of
Salt River near Roosevelt, AZ, USA (saltriver data).
}
\value{
Returns a list containing:
   \item{Estimated_values }{Vector containing the estimated function.}
    \item{grid }{The used grid.}
   \item{bw }{Value of the bandwidth.}
}
\references{
Orlecka-Sikora, B. (2008) Resampling methods for evaluating the uncertainty of the
nonparametric magnitude distribution estimation in the probabilistic seismic hazard
analysis. \emph{Tectonophysics} \bold{456}, 38--51.

Quintela-del-Rio, A. (2010) On non-parametric techniques for area-characteristic
 seismic hazard parameters. \emph{Geophysical Journal International}  \bold{180},
  pp. 339--346.
  
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
# Working with earthquake data. We use the catalogue of the National
# Geographic Institute (IGN) of Spain and select the data of the Northwest
# of the Iberian Peninsula.
data(nwip)
require(chron)
require(date)
# we consider the data with magnitude greater than 3
mg<-nwip$magnitude[nwip$magnitude>3.0]
x1<-nwip$year
x2<-nwip$month
x3<-nwip$day
ys<-paste(x1,x2,x3)
earthquake_date<-as.character(ys)
y1s<-as.date(earthquake_date, order = "ymd")
# we compute the total number of years
y2s<-as.POSIXct(y1s)
z<-years(y2s)
n.years<-length(levels(z))
# the mean rate of earthquakes per year
lambda<-length(mg)/n.years
  \dontrun{
# we estimate the mean return period (in years) between earthquakes of 
# the same magnitude
est2<-mrp(vec_data=mg, lambda=lambda)
plot(est2$grid, est2$Estimated_values, type="l", 
xlab="magnitude", ylab="Mean return period (years)")
 }
##  Working with hydrological data: annual peak instantaneous flow of the
# Salt River near Roosevelt, AZ, USA, for 1924 - 2006.
data(saltriver)
peak<-saltriver$peakflow
\dontrun{
year<-saltriver$year
plot(year,peak, type="l",ylab="Annual peak flow")
 }
 #  mean return period for the Saltriver data 
rp<-mrp(type_kernel="n", vec_data=peak, lambda=1)
 \dontrun{
plot(rp$grid, rp$Estimated_values, type="l",
xlab="Flow level", ylab="Years ", main="Mean return period")
}
}
\keyword{ smooth }
\keyword{ nonparametric}
