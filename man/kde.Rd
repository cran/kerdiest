\name{kde}
\alias{kde}

\title{
Kernel estimator of the distribution function
}
\description{
Computes the value of the kernel estimator of the distribution function, in a
 single value or in a grid. Four possibilites for the kernel function are 
 implemented, and the bandwidth parameter can be directly calculated by the
 plug-in method of Polansky and Baker (2000).
}
\usage{
kde(type_kernel = "n", vec_data, y = seq(min(vec_data), max(vec_data),
 length.out = 100), bw = PBbw(type_kernel = "n", vec_data, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type_kernel}{  The kernel function. You can use 
  four types:   "e" Epanechnikov,	"n" Normal,  "b" Biweight and
 "t" Triweight.  The Normal kernel is used by default.
}
  \item{vec_data}{  The data sample.   }

  \item{y}{The single value or the grid vector where we estimate the
   distribution function. By default, a grid of 100 equidistant points from the minimum 
   to the maximum of the data sample is selected.
}
  \item{bw}{The bandwidth used. If it is not provided, the Plug-in bandwidth 
  of Polansky and Baker (2000) is computed.
}
}

\value{   Returns a list containing:
  \item{Estimated_values }{Vector containing the estimated function in the grid values.}
  \item{grid }{The used grid.}
   \item{bw }{Value of the bandwidth.}
}
 \references{
Reiss, R.D. (1981) Nonparametric estimation of smooth distribution functions, 
\emph{Scandinavian Journal of Statistics} \bold{8}, pp:116-119.

Simonoff, J. (1996) \emph{Smoothing Methods in Statistics}, Springer, New York.

Polansky, A.M. and Baker, E.R. (2000) Multistage plug-in bandwidth selection 
for kernel distribution function estimates, \emph{Journal of Statistical 
Computation and Simulation} \bold{65}, pp. 63-80.
}
\author{ Graciela Estevez Perez \email{graci@udc.es} and Alejandro Quintela del Rio 
\email{aquintela@udc.es}
}

\examples{
# comparison of three bandwidth selection methods

x<-rnorm(100)
# we calculate the bandwidth by cross-validation, plug-in of Altman and Leger
# and plug-in of Polansky and Baker, using a normal kernel and a standard
# setting of parameters, in each case
h_CV<-CVbw(vec_data=x)$bw
# plug-in of Altman and Leger
h_AL<- ALbw(vec_data=x)
# plug-in of Polansky and Baker
h_PB<- PBbw(vec_data=x)
\dontrun{print(h_CV); print(h_AL); print(h_PB)
# plot of the three estimates together with the real distribution
F_CV<-kde(vec_data=x, bw= h_CV)
F_AL<-kde(vec_data=x, bw= h_AL)
F_PB<-kde(vec_data=x, bw= h_PB)
y<-F_CV$grid
Ft<-pnorm(y)
# plot of the theoretical and estimated distribution functions
 require(graphics)
plot(y,Ft, ylab="Distribution", xlab="data", type="l", lty=1)
lines(y,F_CV$Estimated_values, type="l",lty=2)
lines(y,F_AL$Estimated_values, type="l",lty=3)
lines(y,F_PB$Estimated_values, type="l",lty=4)

legend(1,0.4,c("real","F_CV","F_AL","F_PB"),lty=1:4)  }
}
\keyword{ smooth }
\keyword{ nonparametric}