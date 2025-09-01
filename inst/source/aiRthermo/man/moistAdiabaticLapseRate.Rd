\name{moistAdiabaticLapseRate}
\alias{moistAdiabaticLapseRate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moist Adiabatic Lapse Rate 
}
\description{
This function calculates the moist adiabatic lapse rate according to a provided mixing ratio (kg/kg) (Tsonis, eq 7.29).
}
\usage{
moistAdiabaticLapseRate(w, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
  A vector with mixing ratio values (kg/kg).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}

\value{
This function returns a vector with the moist adiabatic lapse rate (dry adiabatic lapse rate with correction of \eqn{c_p} due to the water vapour in moist air).
}
\references{
Tsonis, A. A. (2002). An Introduction to Atmospheric Thermodynamics, Cambridge University Press, Cambridge. Eq. 7.29.
}


\examples{
data(RadiosondeA)
aws<-RadiosondeA[,6]/1000
moistAdiabaticLapseRate(aws)
}
\keyword{Functions}