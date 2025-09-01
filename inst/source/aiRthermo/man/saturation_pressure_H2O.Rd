\name{saturation_pressure_H2O}
\alias{saturation_pressure_H2O}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saturation Pressure
}
\description{
This function returns the saturation pressure (Pa) from a given array of temperatures (K). It uses approximate equations 5.67 and 5.70 in Bohren Albrecht, 1998.
}
\usage{
saturation_pressure_H2O(Temps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Temps}{
A vector with temperature values in Kelvin.
}
}
\details{
Saturation pressure of water vapour \eqn{e_s} is computed over ice/water depending whether the temperature is over/under 273.15 K (0 degree Celsius) at every element of the array.
}
\value{
This function returns a vector with saturation pressure values (Pa).
}
\references{
Bohren, C.F., & Albrecht, B. A. (1998). Atmospheric thermodynamics. Atmospheric thermodynamics. Publisher: New York; Oxford: Oxford University Press, 1998. ISBN: 0195099044. Equations 5.67 and 5.70. 
}

\examples{
data(RadiosondeA)
aTs<-C2K(RadiosondeA[,3])
esats<-saturation_pressure_H2O(aTs)
}
\keyword{Functions}