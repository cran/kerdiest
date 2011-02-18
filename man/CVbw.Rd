\name{CVbw}
\alias{CVbw}

\title{
Computes the cross-validation bandwidth of 
Bowman et al (1998).
}
\description{
The bandwidth parameter for the distribution function kernel estimator is calculated,
using the modified cross-validation method of Bowman, Hall and Prvan (1998). Four possible
kernel functions can be used:  "e" Epanechnikov, "n" Normal, "b" Biweight and
"t" Triweight.    The cross-validation function involves an integral term, that is
 calculated using the Simpson's rule.
}
\usage{
CVbw(type_kernel = "n", vec_data, n_pts = 100, seq_bws =
 seq((max(vec_data) - min(vec_data))/200,
 (max(vec_data) - min(vec_data))/2, length.out = 50))
}
 \arguments{
  \item{type_kernel}{  The kernel function. You can use 
  four types:   	"e" Epanechnikov,	"n" Normal, "b" Biweight and
"t" Triweight.  The normal kernel is used by default.
}
  \item{vec_data}{
The data  sample.
}
  \item{n_pts}{
The number of points used to approximate, by the Simpson's rule, the integral term, 
 in the cross-validation function. 
Because this numeric method enlarge the computing time, you can check different numbers,
depending on your sample size. 100 points are used by default.
}
  \item{seq_bws}{
The sequence of bandwidths in which to compute the cross-validation function. 
By default, a grid consisting in a sequence of 50 points is constructed, from the 
range of the data divided by 200 to the range divided by 2.
}
  }

\value{ A list consisting of 
  \item{seq_bws}{The sequence of bandwidths.}
  \item{CV function }{The values of the cross-validation function in the bandwidths grid.}
  \item{bw}{ A real value for the cross-validation bandwidth.}

}
\references{
Bowman, A.W., Hall, P. and Prvan,T. (1998) Cross-validation for the smoothing of 
distribution functions, \emph{Biometrika} \bold{85}, pp. 799-808.
}
\author{ Graciela Estevez Perez \email{graci@udc.es} and Alejandro Quintela del Rio 
\email{aquintela@udc.es}
}
\examples{
##  Compute the cross-validation bandwidth for a sample of 100 random N(0,1) data
x<-rnorm(100,0,1)
num_bws <- 50
seq_bws <- seq(((max(x)-min(x))/2)/50,(max(x)-min(x))/2,length=num_bws) 
hCV <- CVbw(type_kernel="e", vec_data=x, n_pts=200, seq_bws=seq_bws)
hCV
##  The CV function is plotted
h_CV<-CVbw(vec_data=x)
h_CV$bw
plot(h_CV$seq_bws, h_CV$CVfunction, type="l")
\dontrun{
##  Plotting the distribution function estimate controling the grid points 
## and the kernel function
ss <- quantile(x, c(0.05, 0.95))
# number of points to be used in the representation of estimated distribution
# function
n_pts <- 100  
y <- seq(ss[1],ss[2],length.out=n_pts)
F_CV<-kde(type_kernel="e", x, y, h_CV$bw)$Estimated_values
##  plot of the theoretical and estimated distribution functions
require(graphics)
plot(y,F_CV, type="l", lty=2)
lines(y, pnorm(y),type="l", lty=1)
legend(-1,0.8,c("real","nonparametric"),lty=1:2)
}
}
\keyword{ smooth }
\keyword{ nonparametric }
