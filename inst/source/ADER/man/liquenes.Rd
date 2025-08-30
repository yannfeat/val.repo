\name{liquenes}
\alias{liquenes}
\docType{data}
\title{
Epiphytic Lichens Data
}
\description{
Percentage cover of foliose and fruticulose epiphytic lichen species in 20 cm x 20 cm quadrats placed on the trunk of several trees along three transects in a pine forest in Sierra de Guadarrama (Madrid, Spain). 
}
\usage{data("liquenes")}
\format{
  A data frame with 608 observations on the following 33 variables.
  \itemize{
    \item \code{Grupo}. Transect identifier.
    \item \code{Arbol}. Tree identifier within each transect.
    \item \code{DAP}. DBH, i.e., diameter (cm) at breast height of the phorophyte tree.
    \item \code{Especie}. Species of the phorophyte tree. A factor with levels \code{P} (\emph{Pinus sylvestris}) and \code{R} (\emph{Quercus pyrenaica}).
    \item \code{Distancia}. Distance (m) from the tree to a road on the border of the forest.
    \item \code{Sitio}. Location on the trunk. A factor with levels \code{B} (base of the tree) or \code{P} (at breast height).
    \item \code{Orientacion}. Orientation of the quadrat over the tree trunk. A factor with levels \code{N} (facing North) or \code{S} (facing South).
    \item \code{Musgos}. Percentage cover of mosses.
    \item \code{Crustaceos}. Percentage cover of crustaceous lichens.
    \item \code{Agrietacion}. A measure (percentage) of bark rugosity.
    \item \code{Cetraria.chlorophylla} to \code{Sp.1}. Percentage cover of each recorded species of foliose or fruticulose lichen.
    \item \code{Riqueza}. Richness of fruticulose and foliose lichen species per quadrat.
  }
}
\details{
Data collected by students of Ecology of Rey Juan Carlos University in September 2016. 
}
\examples{
data(liquenes)
str(liquenes)
}
\keyword{datasets}
