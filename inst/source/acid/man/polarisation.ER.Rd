\name{polarisation.ER}
\alias{polarisation.ER}

\title{  Polarisation Measure from Esteban and and Ray (1994)
}
\description{ This function computes the polarisation measure proposed in Esteban and and Ray (1994).
}
\usage{
polarisation.ER(alpha, rho, comp = FALSE)
}

\arguments{
  \item{alpha}{ a scalar containing the alpha parameter from Esteban and Ray (1994) on the sensitivity to polarisation.
}
  \item{rho}{ a dataframe with the group means in the first column and their respective population shares in the second. 
  The groups need to be exogenously defined. 
  
  Note: the two columns should be named \code{means} and ''shares'' respectively. Otherwise a warning will appear. 
}
  \item{comp}{ logical; if TRUE, all components pf p_i^(a+alpha)*p_j*abs(y_i-y_j)
%%     ~~Describe \code{comp} here~~
}
}
\value{
\item{P}{the polarisation measure proposed by Esteban and Ray (1994).}
\item{means}{the means stored in rho.}
\item{shares}{the shares stored in rho..}
\item{ERcomp}{if comp is TRUE, the components aggregated in P.}
}
\references{
Esteban, J. and Ray, D. (1994): On the Measurment of Polarization, in: Econometrica, Vol. 62(4), pp. 819-851.
}
\author{   Alexander Sohn
}


\seealso{ \code{\link{polarisation.EGR}}
}
\examples{
means<-rnorm(10)+5
shares<-  rep(1,length(means))
shares<-shares/sum(shares)
rho<-data.frame(means=means,shares=shares)
alpha<-1
polarisation.ER(alpha,rho,comp=FALSE)
}