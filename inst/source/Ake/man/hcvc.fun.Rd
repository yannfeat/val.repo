\encoding{UTF-8}
\name{hcvc.fun}
\alias{hcvc.fun}
\alias{hcvc.fun.default}
\title{
Cross-validation function for bandwidth selection for continuous data
}
\description{
The (S3) generic function \code{hcvc.fun} computes the 
 cross-validation bandwidth selector.
  
}
\usage{
hcvc.fun(Vec,\dots)
\method{hcvc.fun}{default}(Vec, bw = NULL, type_data, ker, a0 = 0, a1 = 1, \dots)
}

\arguments{
   \item{Vec}{The data sample from which the estimate is to be computed.

}
  \item{bw}{
The sequence of bandwidths where to compute the cross-validation. Default value is NULL.
}
  \item{type_data}{ The sample data type.

}
  \item{ker}{ The associated kernel.

}
  \item{a0}{ The left bound of the extended beta. Default value is 0.

}
  \item{a1}{The right bound of the extended beta.Default value is 1.
}
  \item{\dots}{ Further arguments.
}
}
\details{
\code{hcvc.fun} implements the choice of the bandwidth \eqn{h} using the cross-validation approach
  of a kernel density estimator.
}
\value{
Returns a list containing:
 \item{hcv}{value of bandwidth parameter.}
  \item{CV}{the values of cross-validation function.}
  \item{seq_h}{the sequence of bandwidths where the cross validation is computed.}
}
\references{
Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

Chen, S. X. (2000). Gamma kernels estimators for density functions, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.

Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.
}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji
}

\examples{
V=rgamma(100,1.5,2.6)
\dontrun{
hcvc.fun(V,NULL,"continuous","GA")
}
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
