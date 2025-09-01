\name{TTheta2P}
\alias{TTheta2P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pressure from temperature and potential temperature
}
\description{This function calculates the pressure from given potential temperature and temperature, assuming a dry adiabatic evolution (mixing ratio is only used to correct the values of \eqn{c_p}).
}
\usage{
TTheta2P(Temp, Theta, w = 0, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Temp}{
  A vector with temperature values in Kelvin.
}
  \item{Theta}{
A vector with potential temperature values in Kelvin.
}
  \item{w}{
Initial value of mixing ratio (kg/kg). By default 0.
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}
\value{
  This function returns a vector with pressure values.
}

\examples{
	data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dTds<-w2Td(dPs,dws)
dThetas<-PT2Theta(dPs,dTs)
TTheta2P(dTs,dThetas)

}
\keyword{Functions}