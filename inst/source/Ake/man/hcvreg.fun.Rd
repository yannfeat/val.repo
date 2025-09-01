\name{hcvreg.fun}
\alias{hcvreg.fun}
\alias{hcvreg.fun.default}
\title{
Cross-validation function for bandwidth selection in regresssion
}
\description{
The (S3) generic function \code{hcvreg.fun} computes the bandwidth by cross-validation for the regression.
 Its default method does so. It allows to compute the optimal bandwidth using the cross-validation method. The associated kernels available are: "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian, DiracDU, binomial and discrete triangular; see Kokonendji and Senga Kiessé (2011), and also Kokonendji et al. (2009).

}
\usage{
hcvreg.fun(Vec, \dots)
\method{hcvreg.fun}{default}(Vec, y, type_data = c("discrete", "continuous"), 
ker = c("bino", "triang", "dirDU", "BE", "GA", "LN", "RIG"),
 h = NULL, a0 = 0, a1 = 1, a = 1, c = 2, \dots)
}

\arguments{
  \item{Vec}{
The explanatory variable.
}
  \item{y}{
The response variable.
}
  \item{type_data}{
The data sample type. Data can be continuous or discrete.
}
  \item{ker}{
A character string giving the smoothing kernel to be used which is the associated kernel: "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian, "dirDU" DiracDU,"bino" binomial, "triang" discrete triangular.
}
  \item{h}{
The bandwidth or smoothing parameter.the smoothing bandwidth to be used, can also be a character 
          string giving a rule to choose the bandwidth.
}
  \item{a0}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
  \item{a1}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}
  \item{a}{
The arm of the discrete triangular kernel
}
  \item{c}{
The number of categories
}
  \item{\dots}{Further arguments

}
}
\details{
The selection of the bandwidth parameter is always crucial. If the bandwidth is small, we will obtain an
undersmoothed estimator, with high variability. On the contrary, if the value  is big, the
resulting estimator will be very smooth and farther from the function that we are trying to estimate.
The cross-validation function defined in the above sections is used to compute the optimal bandwidth for the associated kernels. 
}
\value{
Returns a list containing:
\item{kernel}{The associated kernel used to compute the optimal bandwidth.  }
\item{hcv}{The optimal bandwidth parameter obtained by cross-validation.}
\item{CV}{The values of the cross-validation.}
\item{seq_bws}{A sequence of bandwidths where the cross-validation is computed.}
}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
 \emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Demétrio, C.G.B. (2009). Appropriate kernel regression on a count explanatory variable and applications,
\emph{Advances and Applications in Statistics}  \bold{12},  99 - 125.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji 
}
\examples{
## Data can be simulated data or real data
## We use real data 
## and then compute the cross validation. 
data(milk)
x=milk$week
y=milk$yield
hcvreg.fun(x,y,"discrete",ker="triang",a=1)

}
\keyword{smooth}
\keyword{nonparametric}
