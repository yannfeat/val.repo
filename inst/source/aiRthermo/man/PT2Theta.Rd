\name{PT2Theta}
\alias{PT2Theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Potential Temperature from pressure and temperature
}
\description{
This function calculates the potential temperature from given temperature and pressure.
}
\usage{
PT2Theta(P, Temp, w = 0, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values (Pa).

}
  \item{Temp}{
A vector with temperature values (K).
}
  \item{w}{
A vector with mixing ratio values (kg/kg). Default value 0.
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
This function returns a vector with potencial temperature. Mixing ratio is only used to correct the value of \eqn{c_p}, not to calculate a moist adiabatic evolution.
}

\seealso{
\code{\link{moistCp}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dThetas=PT2Theta(dPs,dTs)
}
\keyword{Functions}