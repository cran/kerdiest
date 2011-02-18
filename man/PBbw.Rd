\name{PBbw}
\alias{PBbw}

\title{Computes the plug-in bandwidth of Polansky and Baker.
}
\description{
 The bandwidth parameter for the distribution function kernel estimator is calculated,
using the plug-in method of Polansky and Baker (2000). Four possible kernel 
functions can be used for the kernel estimator:  "e" Epanechnikov,	"n" Normal, 
"b" Biweight and "t" Triweight. A kernel estimator of the second 
derivative of the distribution is needed, and only the kernel normal is used in
this case.
}
\usage{
PBbw(type_kernel = "n", vec_data, num_stage = 2)
}
\arguments{
  \item{type_kernel}{
The kernel function used. You can use 
  four types:   "e" Epanechnikov,	"n" Normal,  "b" Biweight and
 "t" Triweight. The kernel normal is used by default.
}
  \item{vec_data}{
The data sample.
}
  \item{num_stage}{
The number of iterations in the Polansky and Baker's method. b=2 is usually a
good option, and this is the value by default.
}
}

\value{ A real value for the bandwidth parameter.
}
\references{
Polansky, A.M. and Baker, E.R. (2000) Multistage plug-in bandwidth selection 
for kernel distribution function estimates, \emph{Journal of Statistical 
Computation and Simulation} \bold{65}, pp. 63-80.
}
\author{ Graciela Estevez Perez \email{graci@udc.es} and Alejandro Quintela del Rio 
\email{aquintela@udc.es}
}

\examples{
# Compute the plug-in bandwidth for a sample of 100 random N(0,1) data
x<-rnorm(100,0,1)
h_PB<-PBbw(vec_data=x,num_stage=4)
h_PB
\dontrun{
## A Quick plot of a distribution function estimate
x<-rnorm(1000)
h_PB<-PBbw(vec_data=x)
F_PB<-kde(vec_data=x, bw=h_PB)
plot(F_PB$grid, F_PB$Estimated_values, type="l")
}
\dontrun{
##  Plotting the distribution function estimate controling the grid points and 
## the kernel function
ss <- quantile(x, c(0.05, 0.95))
# number of points to be used in the representation of the estimated
# distribution function
n_pts <- 100  
y <- seq(ss[1],ss[2],length.out=n_pts)
F_PB <- kde(type_kernel="e", x, y, h_PB)$Estimated_values
##  plot of the theoretical and estimated distribution functions
require(graphics)
plot(y,F_PB, type="l", lty=2)
lines(y, pnorm(y),type="l", lty=1)
legend(-1.2,0.8,c("real","nonparametric"),lty=1:2)
}
}
\keyword{ smooth }
\keyword{ nonparametric}
