\name{C2K}
\alias{C2K}

\title{
From Celsius to Kelvin degrees
}
\description{
This function makes the transformation from Celsius to Kelvin degrees.
}
\usage{
C2K(Tc, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tc}{
  A vector of temperatures in degrees Celsius.
}
  \item{consts}{
  This funtion uses the \eqn{T_0} constant, corresponding to 0 degree Celsius expressed in K (273.15 K).
}
}
\value{
A vector of temperatures in Kelvin degrees is returned.
}

\seealso{
\code{\link{aiRthermoConstants}} and \code{\link{K2C}}
}
\examples{
data(RadiosondeD)
dTs<-RadiosondeD[,3]
C2K(dTs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}