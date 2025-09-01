\encoding{UTF-8}
\name{dke.fun}
\alias{dke.fun}
\alias{dke.fun.default}
\title{
Function for density estimation
}
\description{
The (S3) generic function \code{dkde.fun} computes the density.
 Its default method does so with the given kernel 
  and bandwidth \eqn{h}.
}
\usage{
dke.fun(Vec, \dots)
\method{dke.fun}{default}(Vec, h, type_data = c("discrete", "continuous"), 
ker = c("BE", "GA", "LN", "RIG"), x = NULL, a0 = 0, a1 = 1, \dots )
}

\arguments{

\item{Vec}{
The data sample from which the estimate is to be computed.
}
  \item{h}{
The bandwidth or smoothing parameter.
}
  \item{type_data}{
The data sample type. Data can be continuous or discrete (categorical or count). Here, in this function , we deal with continuous data.
}
  \item{ker}{
A character string giving the smoothing kernel to be used which is the associated kernel: "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian.
}
  \item{x}{
The points of the grid at which the density  is to be estimated.
}
  \item{a0}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
  \item{a1}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}
  \item{\dots}{
Further arguments.
}
}
\details{
The associated  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined in the above sections.
We recall that in general, the sum of the estimated values on the support is not equal to 1. In practice, we compute the global normalizing constant \eqn{C_n} before computing the estimated density \eqn{\tilde{f}_n}{\tilde{f}_n}; see e.g. Libengué (2013).
}
\value{
Returns a list containing:
\item{data}{The data - same as input Vec.}
  \item{n}{The sample size.}
  \item{kernel}{The asssociated kernel used to compute the density estimate.}
  \item{h}{The bandwidth used to compute the density estimate.}
  \item{eval.points}{The coordinates of the points where the 
  density  is estimated.}
  \item{est.fn}{The estimated density values.}
 \item{C_n}{The global normalizing constant.}
 \item{hist}{The histogram corresponding to the observations.}
}
\references{
Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji 
}

\examples{
## A sample data with n=100.
V<-rgamma(100,1.5,2.6)
##The bandwidth can be the one obtained by cross validation.
h<-0.052
## We choose Gamma kernel.

est<-dke.fun(V,h,"continuous","GA")
}
\keyword{smooth}
\keyword{nonparametric}
