\name{moistCp}
\alias{moistCp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moist Cp
}
\description{
This function corrects the value of dry \eqn{c_p} due to the existence of water vapour acording to equation 7.11 from Tsonis (2002).
}
\usage{
moistCp(w, consts = export_constants())
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
This function returns the value of dry \eqn{c_p} corrected by the mixing ratio.
}
\references{
Tsonis, A. A. (2002). An Introduction to Atmospheric Thermodynamics, Cambridge University Press, Cambridge. Eq. 7.11.
}
\seealso{
\code{\link{w2q}} and 
\code{\link{moistCv}}
}
\examples{
data(RadiosondeD)
dws<-RadiosondeD[,6]/1000
moistCp(dws)
}
\keyword{Functions}