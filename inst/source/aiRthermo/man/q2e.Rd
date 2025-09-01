\name{q2e}
\alias{q2e}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partial Vapour Pressure 
}
\description{
This function calculates the partial vapour pressure from specific humidity.
}
\usage{
q2e(P, q, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values (Pa).
}
  \item{q}{
A vector with specific humidity values (kg/kg).
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}
\value{
This function returns the value of the partial vapour pressure (Pa).
}

\examples{
# Get partial pressure of water vapour
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dws<-RadiosondeD[,6]/1000
h2oe<-q2e(dPs,w2q(dws))
}
\keyword{Functions}