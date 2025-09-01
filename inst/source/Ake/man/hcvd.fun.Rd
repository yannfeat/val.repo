\name{hcvd.fun}
\alias{hcvd.fun}
\alias{hcvd.fun.default}
\title{
Cross-validation function for bandwidth selection in p.m.f. estimation
}
\description{
The (S3) generic function \code{hcvd.fun} computes the
 cross-validation bandwidth selector in p.m.f. estimation.
}
\usage{
hcvd.fun(Vec, \dots)
\method{hcvd.fun}{default}(Vec, seq_bws = NULL, ker = c("bino", "triang", "dirDU"), a = 1, c = 2,\dots)
}
  
\arguments{
   \item{Vec}{The data sample from which the estimate is to be computed.

}
  \item{seq_bws}{
The sequence of bandwidths where to compute the cross-validation. Default value is NULL.
}
  \item{ker}{ The associated  kernel

}
  \item{a}{ The  arm of the discrete triangular kernel. Default value is 1.

}
  \item{c}{The number of categories in DiracDU kernel.Default value is 2.
}
  \item{\dots}{ Further arguments.
}
}
\details{
The \code{hcvd.fun} function implements the choice of the bandwidth \eqn{h} using the cross-validation approach in p.m.f. estimate. 
 
}
\value{
Returns a list containing:
 \item{hcv}{The optimal bandwidth parameter.}
  \item{CV}{The cross-validation function values.}
  \item{seq_h}{The sequence of bandwidths where the cross-validation is computed.}
}
\references{
Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.


Chen, S. X. (2000). Probability density function estimation using gamma kernels, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.


Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.
}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji
}


\examples{
## Data can be simulated data or real data
## We use real data 
## and then compute the cross validation. 
Vec<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,
7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,12,12,10,12,12,
13,14,15,16,16,17,0,12)
\dontrun{
CV<-hcvd.fun(Vec,NULL,"bino")
CV$hcv
}
##The cross validation function can be also ploted.
\dontrun{
plot.fun(CV$seq_bws,CV$CV, type="l")
}

}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
