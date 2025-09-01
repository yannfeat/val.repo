\name{gamma_saturated}
\alias{gamma_saturated}

\title{
Saturated Adiabat Gamma
}
\description{
Saturated adiabat at the points of the sounding as computed internally, considering hydrostatic balance and as \eqn{\frac{dT}{dP}} (in pressure levels) (K/Pa).
}
\usage{
gamma_saturated(Ps, Temps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ps}{
    A vector with pressure values (Pa).
}
  \item{Temps}{
    A vector with temperature values (K).
}
}

\value{
This function returns the vertical derivate \eqn{\Gamma _s= \frac{dT}{dP} \Big|_s} for a saturated adiabatic evolution.
}

\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
gamma_saturated(aPs,aTs)
}
\keyword{Functions}