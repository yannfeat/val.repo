\name{PTheta2T}
\alias{PTheta2T}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Temperature from pressure and potential temperature
}
\description{
This function calculates the temperature from a given pressure and potential temperature.
}
\usage{
PTheta2T(P, Theta, w = 0, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values (Pa).
}
  \item{Theta}{
A vector with potential temperature (K).

}
  \item{w}{
A vector with mixing ratio values (kg/kg). Default value 0.
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
This function returns a vector with temperatures (K).
}
\seealso{
\code{\link{moistCp}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dThetas=PT2Theta(dPs,dTs)
PTheta2T(dPs,dThetas)
}
\keyword{Functions}