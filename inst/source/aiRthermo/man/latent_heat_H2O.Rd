\name{latent_heat_H2O}
\alias{latent_heat_H2O}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Latent heat of vaporization or sublimation of water
}
\description{
This function calculates the latent heat of vaporization or sublimation of water depending as a function of temperature. It uses a polynomial approximation over water or ice.
}
\usage{
latent_heat_H2O(Temps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Temps}{
    A vector with temperature values (K).
}
}
\details{
Taking into account the observed values in tables from Rogers and Yau (1989) and Feistel and Wagner (2006), a polynomial model is used to calculate the latent heat at different temperatures.
}
\value{
    This function returns the latent heat of vaporization or sublimation of water.
}
\references{
Rogers, R. R., and  Yau, M. K. (1989). A Short Course in Cloud Physics, 3rd Edition, Pergamon Press, Oxford.

Feistel, R. and Wagner, W. (2006). A new equation of state for H2O ice Ih, Journal of Physical and Chemical Reference Data 35 1021-1047. doi:10.1063/1.2183324.
}
\examples{
data(RadiosondeA)
aTs<-C2K(RadiosondeA[,3])
latent_heat_H2O(aTs) 
}
\keyword{Functions}