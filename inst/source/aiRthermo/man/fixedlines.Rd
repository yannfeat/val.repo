\name{fixedlines}
\alias{fixedlines}
\docType{data}
\title{
Data for plotting the lines of the thermodynamic (STUVE) diagram
}
\description{
The vectors included in the list are: both components of the pseudoadiabatic lines (adiabatic_x_T, and adiabatic_y_T), labels of the pseudoadiabatic lines (adiabatic_z_T), both components of the dry adiabatic lines (theta_x_T and theta_y_T), both components of the constant mixing ratio lines (wsat_x_T and wsat_y_T) and their labels (wsat_z_T). The X components are provided in Celsius and the Y components in hPa. 
}
\details{
The pseudoadiabatic lines were calculated by the authors for this R-package following pseudoadiabatic evolutions from 1050 hPa. 

The dry adiabatic lines were obtained using the functions in \emph{aiRthermo} for different initial conditions and for a fixed set of initial potential temperatures. A similar procedure was applied on the calculation of the constant mixing ratio lines, starting from different values of saturation mixing ratio.
}
\source{
The data were calculated by the authors for this R-package.
}
\usage{data(fixedlines)}

\seealso{
 \code{\link{export_lines}}
}
\examples{
data(fixedlines)
}
\keyword{Datasets}
