\name{AnyAdiabaticDown}
\alias{AnyAdiabaticDown}
\title{
Adiabatic Downwards Evolution
}
\description{
	
Calculation of the state of an air parcel subject to an adiabatic downwards evolution, taking into account the initial conditions of the parcel (Pstart, Tstart, wstart, wcstart).
}
\usage{
AnyAdiabaticDown(Pstart, Tstart, wstart, wcstart, Pend, deltaP)
}

\arguments{
  \item{Pstart}{
Initial pressure value (Pa).
}
  \item{Tstart}{
Initial temperature value (K).
}
  \item{wstart}{
Initial mixing ratio value (kg/kg).
}
  \item{wcstart}{
Initial mixing ratio value for the condensates (kg/kg).
}
  \item{Pend}{
Final pressure value (Pa).
}
  \item{deltaP}{
Pressure step used for the calculation. It must be a positive value (Pa).
}
}
\details{
In this case, we start from a parcel at pressure pstart (Pa), temperature tstart (K) and mixing ratio wstart (kg/kg), with potentially some condensates wcstart (kg/kg). The latent heat (L) used during the evolution depends on the Temperature (T). It is computed as described by \code{\link{latent_heat_H2O}}. As the parcel goes down it could evaporate the condensates or, if no condensates are available anymore, it will go down according to a dry adiabatic evolution by means of a dry adiabatic process until the level Pend. At this point, it will have a temperature Tend, mixing ratio (vapour) Wend  and Wcend (may be still some condensates could be left) using steps of pressure dP (always positive).
}
\value{
This function returns a list including the following values:
\item{Tend}{Temperature at the end (K).} 
\item{Wend}{Mixing ratio of water vapour at the end (kg/kg).}
\item{Wcend}{Mixing ratio of condensed water at the end (kg/kg).}
}

\examples{
AnyAdiabaticDown(50000,227,8.5e-5,0.005,101325,5)
AnyAdiabaticDown(70000,237,4e-4,0.005,101325,5)
}
\keyword{Functions}