\name{saltriver}
\alias{saltriver}
\docType{data}
\title{
Data from the
Salt River near Roosevelt, AZ, USA, for 1924-2006.
}
\description{
The annual peak instantaneous flow of the Salt River near Roosevelt, AZ, USA, 
for 1924-2009. Data are in cfs (0.028317 m3/s); water year October-September. 
The data were examined in several papers related with extreme values in
 hydrology. Among others, they were analyzed by Anderson and Meerschaert (1998)
  and Dettinger and Diaz (2000), where they were fitted to a GEV and a GPD 
distribution. In Quintela del Rio (2011), a nonparametric analysis for this
data set is made.
}
\usage{data(saltriver)}
\format{
  A data frame with 85 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{Year}
    \item{\code{peakflow}}{The annual observed maximum peak flow}
  }
}

\source{
US Geological Survey http://water.usgs.gov/nwis/peak.
}
\references{
Anderson, P.L. and Meerschaert, M.M. (1998) Modeling river flows with heavy
tails, \emph{Water Resources Research} \bold{34}, pp. 2271--2280.

Dettinger, M.D. and Diaz, H.F. (2000) Global characteristics of stream flow
seasonality and variability, \emph{Journal of Hydrometeorology} \bold{1}, pp. 
289--310.

Quintela-del-Rio, A. (2011)  On bandwidth selection for nonparametric estimation
  in flood frequency analysis. \emph{Hydrological Processes}  \bold{25},
  pp. 671--678.

Quintela-del-Rio, A. and Estevez-Perez, G. (2012) 
Nonparametric Kernel Distribution Function Estimation with kerdiest: 
An R Package for Bandwidth Choice and Applications, 
\emph{Journal of Statistical Software} \bold{50(8)}, pp. 1-21.
 URL http://www.jstatsoft.org/v50/i08/.

}
\examples{
data(saltriver)
peak<-saltriver$peakflow
year<-saltriver$year
plot(year,peak, type="l",ylab="Annual peak flow")
}
\keyword{datasets}
