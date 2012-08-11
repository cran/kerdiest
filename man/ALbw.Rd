\name{ALbw}
\alias{ALbw}
\title{  Computes the plug-in bandwidth of Altman and Leger.
}                              
\description{ The bandwidth parameter for the distribution function kernel 
estimator is calculated, using the plug-in method of Altman and Leger (1995). 
Four possible kernel functions can be used for the kernel estimator:  "e"
 Epanechnikov,	"n" Normal,  "b" Biweight and  "t" Triweight.  
 }
 \usage{
ALbw(type_kernel = "n", vec_data)
}
\arguments{
  \item{type_kernel}{  The kernel function. You can use 
  four types:   "e" Epanechnikov,	"n" Normal,  "b" Biweight and
 "t" Triweight.  The Normal kernel is used by default.
}
  \item{vec_data}{
The data sample. 
}
}
\value{ A real value for the bandwidth parameter.
}
\details{
Altman and Leger (1995) recommend the use of the Epanechnikov kernel, because
in this case the rate of convergence for the kernel derivative estimator is 
improved. For the sake of uniformity along the package, the gaussian kernel is 
used by default, but the user can obviously choose the Epanechnikov function.
 }
\references{ Altman, N., Leger, C. (1995) Bandwidth selection for kernel distribution 
function estimation. \emph{Journal of Statistical Planning and Inference}
 \bold{46}, pp. 195--214. 

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
# Compute the plug-in bandwidth for a sample of 100 random N(0,1) data
x<-rnorm(100,0,1)
h_AL<- ALbw(type_kernel="e",vec_data=x)
h_AL
\dontrun{
## A Quick plot of a distribution function estimate
x<-rnorm(1000)
h_AL<-ALbw(vec_data=x)
F_AL<-kde(vec_data=x, bw=h_AL)
plot(F_AL$grid,F_AL$Estimated_values,type="l")
}
\dontrun{
##  Plotting the distribution function estimate controling the grid points
# and the kernel function
ss <- quantile(x, c(0.05, 0.95))
# number of points to be used in the representation of estimated distribution 
# function
n_pts <- 100 
y <- seq(ss[1],ss[2],length.out=n_pts)
F_AL <- kde(type_kernel="e", x, y, h_AL)$Estimated_values
##  plot of the theoretical and estimated distribution functions
require(graphics)
plot(y,F_AL, type="l", lty=2)
lines(y, pnorm(y),type="l", lty=1)
legend(-1,0.8,c("real","nonparametric"),lty=1:2)
}
}
\keyword{ smooth }
\keyword{ nonparametric}
