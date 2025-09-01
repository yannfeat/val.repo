\name{adiabatic_ascent}
\alias{adiabatic_ascent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Properties of an air parcel after adiabatic ascent 
}
\description{
A particle located at Pstart pressure (Pa), Tstart temperature (K) and wstart mixing ratio (kg/kg) ascends (pseudo)adiabatically to Pend (Pa). The evolution is computed by numerically integrating the dT/dP ordinary differential equation (ODE) using a 4th order Runge-Kutta scheme, assuming hydrostatic equilibrium and that the particle is saturated after the Lifted Condensation Level (LCL).
}
\usage{
adiabatic_ascent(Pstart, Tstart, wstart, Pend, deltaP = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Pstart}{
   Initial value for pressure (Pa).
}
  \item{Tstart}{
   Initial value for temperature (K).
}
  \item{wstart}{
  Initial value for mixing ratio (kg/kg).
}
  \item{Pend}{
   End value for pressure (Pa).
}
  \item{deltaP}{
  deltaP (Pa) represents the numerical increment used for integrating the Ordinary Differential Equation (ODE) representing the vertical evolution.
}
}

\value{
The function returns a list that includes Tend (final value of temperature) and mixRatioEnd (mixing ratio of the air parcel at the end of the evolution).
  \item{Tend}{
  Temperature at the end (K).
}
  \item{mixRatioEnd}{
  Mixing ratio at the end (kg/kg).
}

}

\examples{
P0<-101325
T0<-273.15
w0<-0.0025
adiabEvov<-adiabatic_ascent(P0,T0,w0,50000,5)
}

\keyword{Functions}