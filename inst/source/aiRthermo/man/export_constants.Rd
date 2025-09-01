\name{export_constants}
\alias{export_constants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export the constants
}
\description{
This function exports to R the constants frecuently used in the C part of \emph{aiRthermo} for consistency.

}

\usage{
export_constants()
}
\details{
The constants stored in the vector are (in SI units): the gas constant for dry air \eqn{R_d} and for water vapour \eqn{R_v} (\eqn{\frac{J}{Kkg}}), the temperature \eqn{T_0} corresponding to 0 degree Celsius, \eqn{es_0} used to calculate the saturated vapour pressure  (Pa), 1000 hPa in Pa (P1000), the specific heat of dry air for constant pressure \eqn{c_p} (\eqn{\frac{J}{Kkg}}) and for constant volume \eqn{c_v} (\eqn{\frac{J}{Kkg}}), acceleration due to gravity at sea level g (\eqn{\frac{m}{s^2}}), our definition of a missing value MISSING_VALUE (-99999999) and epsilon \eqn{\varepsilon} (\eqn{\frac{R_d}{R_v}}).

Constants are taken from Bohren & Albrecht (1998), and they are also consistent with those used in Petty (2008), Erukhimova & North (2009) and Davies-Jones (2009). 
}
\references{
%% Copiado de aiRthermoConstants
Bohren, C.F., & Albrecht, B. A. (1998). Atmospheric thermodynamics. Atmospheric thermodynamics. Publisher: New York; Oxford: Oxford University Press, 1998. ISBN: 0195099044.

Petty, G.W. (2008). A First Course in Atmospheric Thermodynamics, Sundog Publishing, Madison.

North, G. R. , Erukhimova,T. L. (2009). Atmospheric Thermodynamics, Cambridge University Press, New York.

Davies-Jones, R. (2009). On formulas for equivalent potential temperature, Monthly Weather Review, 137,3137-3148. doi:10.1175/2009MWR2774.1.
}
\seealso{
 \code{\link{aiRthermoConstants}}
}
\examples{
aiRthermoConstants<-export_constants()
}

\keyword{Functions}