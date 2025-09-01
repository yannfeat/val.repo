\name{w2Td}
\alias{w2Td}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dew point temperature from mixing ratio
}
\description{
This function calculates the dew point temperature from given mixing ratio and pressure, following the APPROXIMATE expression 5.68 in Bohren and Albrech (1998).
}
\usage{
w2Td(P, w, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values in Pa.
}
  \item{w}{
A vector with mixing ratio (kg/kg).
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}
\value{
This function returns a vector with the dew point temperature.
}
\references{
Bohren, C.F., & Albrecht, B. A. (1998). Atmospheric thermodynamics. Atmospheric thermodynamics. Publisher: New York; Oxford: Oxford University Press, 1998. ISBN: 0195099044. Equation 5.68.
}

\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dws<-RadiosondeD[,6]/1000
w2Td(dPs,dws)
}
\keyword{Functions}