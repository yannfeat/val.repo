\name{PW}
\alias{PW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vertically integrated water vapour column
}
\description{
This function calculates the vertically integrated water vapour column integrating in pressure vertical coordinates.

}
\usage{
PW(w, PRES, Psurf, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
A vector with mixing ratio values of a sounding (kg/kg).

}
  \item{PRES}{
A vector with pressure values of a sounding (Pa).

}
  \item{Psurf}{
Is the mean sea level pressure at the place of a sounding (Pa).
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}
\value{
This function returns the vertically integrated water vapour column.
}

\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dws<-RadiosondeD[,6]/1000
PW(dws,dPs,dPs[1])
}
\keyword{Functions}