\name{Sindex}
\alias{Sindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Showalter Instability Index 
}
\description{
This function computes Showalter instability index (Celsius) from given parameters from a vertical sounding pressure (Pa), temperature (K) and mixing ratio (kg/kg).
}
\usage{
Sindex(Ps, Ts, ws, deltaP, doLog = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ps}{
A vector with pressure values in Pa.
}
  \item{Ts}{
A vector with temperature values in Kelvin.
}
  \item{ws}{
A vector with mixing ratio values in kg/kg.
}
  \item{deltaP}{
The width (Pa) of the layers used in the numerical solution of the vertical evolution.
}
  \item{doLog}{
Use logarithmic vertical interpolation between sounding levels. A default value is 0.
}
}
\details{

If the needed levels (850 hPa or 500 hPa) are not exactly found in the input sounding, logarithmic/linear vertical interpolation is run to get the corresponding T/w from the Ps/Ts/ws depending on the value of doLog (TRUE or FALSE).

The evolution of the lifted particle is computed by integrating the dT/dP ordinary differential equation (applying the Runge Kutta 4th order method) that represents the vertical adiabatic evolution from 850 hPa to 500 hPa using a pressure step dP > 0 (Pa). The vertical adiabatic evolution is either dry (before saturation) or pseudoadiabatic at every vertical step with a correction for moisture in the specific heat at constant pressure \eqn{c_p} during the dry steps (as in Tsonis, eq 7.11).

If the sounding does not enclose the needed levels and the interpolation fails, the function returns -99999999.
}
\value{
This function returns the Showalter instability index (Celsius).
}
\references{
Djuric D. (1994). Weather Analysis, Prentice Hall, New Jersey.
}
\seealso{
 \code{\link{LIindex}}
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
S<-Sindex(aPs,aTs,aws,5,0)
}
\keyword{Functions}