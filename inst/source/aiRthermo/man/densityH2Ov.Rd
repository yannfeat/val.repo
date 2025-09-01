\name{densityH2Ov}
\alias{densityH2Ov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density of water vapour
}
\description{
From pressure of water vapour Pw (Pa) and temperature Temp (K), this function calculates density of water vapour (\eqn{kg/m^3}).
}
\usage{
densityH2Ov(Pw, Temp, consts = export_constants())
}
\arguments{
  \item{Pw}{
    A vector with pressure water vapour values (Pa).
}
  \item{Temp}{
    A vector with temperature values (K).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}

\value{
A vector with density of water vapour values is returned (\eqn{kg/m^3}).
}

\seealso{
	\code{\link{q2e}} and \code{\link{w2q}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
h2oe<-q2e(dPs,w2q(dws))
densityH2Ov(h2oe,dTs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}