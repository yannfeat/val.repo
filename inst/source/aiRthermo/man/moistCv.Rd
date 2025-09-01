\name{moistCv}
\alias{moistCv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moist cv value
}
\description{
This function is similar to \code{\link{moistCp}} but for \eqn{c_v}. In this case, it is the value of \eqn{c_v} corrected due to the existence of water vapour (equation 7.12) from Tsonis (2002).
}
\usage{
moistCv(w, consts = export_constants())
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
This function returns the value of \eqn{c_v} corrected due to the existence of water vapour.
}
\references{
Tsonis, A. A. (2002). An Introduction to Atmospheric Thermodynamics, Cambridge University Press, Cambridge. Eq. 7.12.
}

\seealso{
\code{\link{w2q}} and \code{\link{moistCp}}
}
\examples{
data(RadiosondeD)
dws<-RadiosondeD[,6]/1000
moistCv(dws)
}
\keyword{Functions}