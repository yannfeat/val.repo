\name{densityMoistAir}
\alias{densityMoistAir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density of Moist Air
}
\description{
From pressure P (Pa) temperature Temp (K) and mixing ratio (kg/kg), this function calculates the density of moist air  (\eqn{kg/m^3}).
}
\usage{
densityMoistAir(P, Temp, w, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
    A vector with pressure values (Pa).
}
  \item{Temp}{
    A vector with temperature values (K).
}
  \item{w}{
  A vector with mixing ratio values (kg/kg).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
A vector with density of moist air values is returned (\eqn{kg/m^3}).
}


\seealso{
	\code{\link{virtual_temperature}}
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
densityMoistAir(aPs,aTs,aws)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}