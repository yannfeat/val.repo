\name{e2w}
\alias{e2w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Mixing Ratio from partial pressure of water vapour
}
\description{
 This function calculates the mixing ratio (kg/kg) from the partial vapour pressure of water vapour (Pa).
}
\usage{
e2w(eh2o, P, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eh2o}{
    A vector with partial pressure of water vapour (Pa).

}
  \item{P}{
    A vector with pressure (Pa) values.
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
A vector with mixing ratio values.
}


\examples{
	
	
#Partial pressure of water vapour
data(RadiosondeA)
dPs<-RadiosondeA[,1]*100
dws<-RadiosondeA[,6]/1000
eh2o<-q2e(dPs,w2q(dws))
#Pressure
e2w(eh2o,dPs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}