\name{find_lcl}
\alias{find_lcl}

\title{
Calculation of the Lifted Condensation Level (LCL)
}
\description{
For a particle with initial conditions \eqn{P_0}, \eqn{T_0} and \eqn{w_0}, this function performs an adiabatic vertical evolution until it gets saturated at most when Ptop is reached.
}
\usage{
find_lcl(Ptop, P0, T0, w0, deltaP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ptop}{
Maximun level pressure selected (Pa).
}
  \item{P0}{
Initial value of pressure (Pa).
}
  \item{T0}{
Initial value of temperature (K).
}
  \item{w0}{
Initial value of mixing ratio (kg/kg).
}
  \item{deltaP}{
The width (Pa) of the layers used in the calculation of the numerical solution for the vertical evolution. A default value of 5 Pa is used.
}
}

\value{
Returns a list including the following values:
\item{Plcl}{The pressure at LCL (Pa).}
\item{Tlcl}{The temperature at LCL (K).}
\item{wlcl}{The mixing ratio at LCL (kg/kg).}
\item{thetalcl}{The potential temperature at LCL (K).}
\item{gotit}{0 or 1 whether the particle arrived or not to saturation (LCL) before arriving to Ptop.}
}
\examples{
Ptop=50000
P0=101325
T0=273.15
w0=0.0025
deltaP=5
rh=100*w0/saturation_mixing_ratio(P0,T0,export_constants())
fndlcl=find_lcl(Ptop,P0,T0,w0,deltaP)
}
\keyword{Functions}