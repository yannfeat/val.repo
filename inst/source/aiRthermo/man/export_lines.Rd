\name{export_lines}
\alias{export_lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export the lines for the thermodynamic diagram
}
\description{
This function exports the \emph{fixedlines} for Stüve Diagram. It includes the data for plotting the pseudoadiabatic (adiabat_*_T), dry adiabatic (theta_*_T) and constant mixing ratio lines (wsat_*_T).
}
\usage{
export_lines()
}
\seealso{
 \code{\link{fixedlines}}
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
stuveA<-stuve_diagram(Pres = aPs/100,Temp=aTs-273.15)
}
\keyword{Functions}
