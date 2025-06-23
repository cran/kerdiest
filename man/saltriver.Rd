\name{saltriver}
\alias{saltriver}
\docType{data}
\title{Data from Salt River
}
\description{The annual peak instantaneous flow of the Salt River near
  Roosevelt, AZ, USA, for 1924-2009. Data are in cfs (0.028317 m3/s);
  water year October-September. The data were examined in several papers
  related with extreme values in hydrology. Among others, they were analyzed
  by Anderson and Meerschaert (1998) and Dettinger and Diaz (2000), where
  they were fitted to GEV and GPD distributions. In Quintela-del-Rio (2011),
  a nonparametric analysis for this data set is presented.
}
\usage{data(saltriver)}
\format{A data frame with 85 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{Year}
    \item{\code{peakflow}}{The annual observed maximum peak flow}
  }
}
\source{US Geological Survey http://water.usgs.gov/nwis/peak.
}
\references{
  Anderson, P.L. and Meerschaert, M.M. (1998), "Modeling river flows with heavy
  tails", Water Resources Research, 34, 2271-2280.

  Dettinger, M.D. and Diaz, H.F. (2000), "Global characteristics of stream flow
seasonality and variability", Journal of Hydrometeorology, 1, 289-310.

  Quintela-del-Rio, A. (2011), "On bandwidth selection for nonparametric
  estimation in flood frequency analysis". Hydrological Processes 25, 671-678.

  Quintela-del-Río, A. and Estévez-Pérez, G. (2012), "Nonparametric kernel
  distribution function estimation with kerdiest: an R package for bandwidth
  choice and applications", Journal of Statistical Software, 50(8), 1-21.
}
\keyword{datasets}
