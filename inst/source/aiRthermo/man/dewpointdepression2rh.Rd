\name{dewpointdepression2rh}
\alias{dewpointdepression2rh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relative Humidity from the dew point depression
}
\description{
This function calculates the relative humidity (\%) from the dew point depression (K).
}
\usage{
dewpointdepression2rh(P, Temp, dpd, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
    A vector with pressure values (Pa).
}
  \item{Temp}{
    A vector with temperature values (K).
}
  \item{dpd}{
    A vector with dew point depression values (K).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}
\value{
A vector with relative humidity (\%).
}
\seealso{
	\code{\link{saturation_mixing_ratio}} and \code{\link{saturation_pressure_H2O}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dTds=w2Td(dPs,dws)
dDPDs=dTs-dTds
dewpointdepression2rh(dPs,dTs,dDPDs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}