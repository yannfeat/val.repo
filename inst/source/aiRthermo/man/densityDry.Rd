\name{densityDry}
\alias{densityDry}

\title{
Density of Dry Air
}
\description{
From pressure P (Pa) and temperature Temp (K), this funtion calculates the density of dry air in \eqn{kg/m^3}.
}
\usage{
densityDry(P, Temp, consts = export_constants())
}

\arguments{
  \item{P}{
    A vector with pressure values (Pa).
}
  \item{Temp}{
    A vector with temperature values (K).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}

\value{
A vector with density of dry air values is returned (\eqn{kg/m^3}).
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
densityDry(dPs,dTs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}