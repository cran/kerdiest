\name{nwip}
\alias{nwip}
\docType{data}
\title{ Earthquakes of the Norwesth of the Iberian Peninsula}
\description{
This data set corresponds to the earthquakes occurred in the Norwesth of the 
Iberian Peninsula, from 25/November/1924 to 31/July/2010. The area is limited 
by the coordinates 41 N -- 44 N and 6 W -- 10 W, involving the autonomic region 
of Galicia (Spain) and northern Portugal.
}
\usage{data(nwip)}
\format{
  A data frame with 3491 observations on the following 10 variables,
  corresponding to the earthquake epicenters and time of ocurrence.
  \describe{
    \item{\code{day}}{Day}
    \item{\code{month}}{Month}
    \item{\code{year}}{Year}
    \item{\code{hour}}{Hour}
    \item{\code{minute}}{Minute}
    \item{\code{second}}{second}
    \item{\code{latitude}}{Latitude in degrees}
    \item{\code{longitude}}{Longitude in degrees}
    \item{\code{depth}}{Depth in km }
    \item{\code{magnitude}}{Magnitude in Richter Scale}
  }
}

\source{
The data catalogue has been obtained from the National Geographic Institute 
(IGN) of Spain. The web page is www.ign.es.

}
\references{
Rueda, J., and J. Mezcua (2001) Sismicidad,  sismotectonica y peligrosidad 
sismica en Galicia, \emph{ IGN Technical Publication} \bold{ 35}.


Quintela-del-Rio, A. and Estevez-Perez, G. (2012) 
Nonparametric Kernel Distribution Function Estimation with kerdiest: 
An R Package for Bandwidth Choice and Applications, 
\emph{Journal of Statistical Software} \bold{50(8)}, pp. 1-21.
 URL http://www.jstatsoft.org/v50/i08/.
}
\examples{
data(nwip)
}
\keyword{datasets}
