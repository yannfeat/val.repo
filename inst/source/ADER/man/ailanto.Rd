\encoding{latin1}
\name{ailanto}
\alias{ailanto}
\docType{data}
\title{
Germination Test of Ailanthus Seeds
}
\description{
Germination frequency of \emph{Ailanthus altissima} seeds in a factorial experiment with varying levels of light and water.
}
\usage{data("ailanto")}
\format{
  A data frame with 48 observations on the following 4 variables.
  \describe{
    \item{\code{Semillas}}{Number of seeds in each tray.}
    \item{\code{Luz}}{Percentage of incident light.}
    \item{\code{Agua}}{Percentage of soil water.}
    \item{\code{Germinacion}}{Number of seeds germinating.}
  }
}
\details{
An experiment with forty-eight trays, with 40 pots each, and one seed per pot, was set up in a factorized design, with four levels of incident light and three levels of humidity. Numbers of germinating seeds by tray were recorded.
}
\source{
\enc{González-Muñoz, N., Castro-Díez, P., and  Fierro-Brunnenmeister, N. 2011. Establishment success of coexisting native and exotic trees under an experimental gradient of irradiance and soil moisture. \emph{Environmental Management} 48: 764-773.}{Gonzalez-Munoz, N., Castro-Diez, P., and  Fierro-Brunnenmeister, N. 2011. Establishment success of coexisting native and exotic trees under an experimental gradient of irradiance and soil moisture. \emph{Environmental Management} 48: 764-773.}
}
\examples{
data(ailanto)
str(ailanto) 
}
\keyword{datasets}
