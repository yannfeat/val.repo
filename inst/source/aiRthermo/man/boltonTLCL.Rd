\name{boltonTLCL}
\alias{boltonTLCL}
\title{
Find the Temperature at the Lifting Condensation Level (LCL) 
}
\description{
This function is used to calculate the Temperature at the Lifting Condensation Level (LCL) using Bolton's approximation instead of integrating the Ordinary Differential Equation (ODE) upwards.}
\usage{
boltonTLCL(TempCelsius, rh, consts = export_constants())
}
\arguments{
  \item{TempCelsius}{
Temperature in degrees Celsius.
}
  \item{rh}{
Relative humidity (\%).
}
  \item{consts}{
 Includes the frecuently used constants in thermodynamics defined in 
 
 \emph{aiRthermoConstants}.
}
}

\value{
This function calculates an approximation of the temperature in degrees Celsius corresponding to the LCL. 
}
\references{
Bolton, D. (1980). The computation of equivalent potential temperature, Monthly Weather Review 108, 1046-1053. doi:10.1175/1520-0493(1980)108<1046:TCOEPT>2.0.CO;2.
}

\examples{
T0=273.15
rh=66.25489
boltonTLCL(T0,rh)
}
\keyword{Functions}