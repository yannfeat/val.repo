\name{virtual_temperature}
\alias{virtual_temperature}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Virtual Temperature
}
\description{
This function calculates the virtual temperature from given pressure and mixing ratio.
}
\usage{
virtual_temperature(P, Temp, w, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values in Pa.
}
  \item{Temp}{
  A vector with temperature values in Kelvin.
}
  \item{w}{
A vector with mixing ratio values in kg/kg.
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
This function returns a vector with virtual temperature values.
}
\seealso{
\code{\link{q2e}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
virtual_temperature(dPs,dTs,dws)
}
\keyword{Functions}