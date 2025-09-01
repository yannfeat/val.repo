\name{K2C}
\alias{K2C}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
From Kelvin to Celsius degrees
}
\description{
This function makes the transformation from Kelvin degrees to Celsius.
}
\usage{
K2C(Tk, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tk}{
  A vector of temperatures in Kelvin degrees.
}
  \item{consts}{
  This function uses the \eqn{T_0} constant corresponding to 0 degree Celsius as K.
}
}

\value{
This function returns a vector of temperatures in Celsius degrees.
}

\seealso{
\code{\link{aiRthermoConstants}} and \code{\link{C2K}}
}
\examples{
data(RadiosondeD)
dTs<-RadiosondeD[,3]
K2C(C2K(dTs))
}
\keyword{Functions}