\name{defo}
\alias{defo}
\docType{data}
\title{
Defoliation in Almeria Pine Forests
}
\description{
Data about defoliation in 76 stands of \emph{Pinus} spp. forests in Sierra de los Filabres, Almeria, Spain.
}
\usage{data("defo")}
\format{
  A data frame with 76 observations on the following 12 variables.
  \describe{
    \item{\code{x}}{UTM x-coordinate of the stand.}
    \item{\code{y}}{UTM y-coordinate of the stand.}
    \item{\code{Especie}}{Dominant species of \emph{Pinus}. A factor with two levels:  \emph{Pinus nigra} and  \emph{Pinus sylvestris}.}
    \item{\code{Defoliacion}}{Percentage of the stand affected by defoliation.}
    \item{\code{Area_basimetrica}}{Basal area of the stand.}
    \item{\code{Altura_media}}{Average height of pines in the stand.}
    \item{\code{Densidad_pinos}}{Density of pine trees in the stand.}
    \item{\code{Elevacion}}{Elevation.}
    \item{\code{Pendiente}}{Average slope.}
    \item{\code{Orientacion}}{Slope orientation.}
    \item{\code{Insolacion}}{Amount of solar insolation.}
    \item{\code{Potencial_hidrico}}{Water potential.}
  }
}

\examples{
data(defo)
str(defo)
}
\keyword{datasets}
