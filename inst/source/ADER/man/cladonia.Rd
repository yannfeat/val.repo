\encoding{latin1}
\name{cladonia}
\alias{cladonia}
\docType{data}
\title{
Cladonia Cover in Finland
}
\description{
Average cover of the lichen \emph{Cladonia rangiferina} (L.) Weber ex F.H.Wigg. in the understory of pine forests in 24 locations in Finland, plus some measurements of soil properties in the same localities.
}
\usage{data("cladonia")}
\format{
  A data frame with 24 observations on the average cover of \emph{Cladonia rangiferina} (\code{cladrang}), the estimated cover of bare soil (\code{Baresoil}), the thickness of the humus layer (\code{Humdepth}) and   measurements of chemical soil characteristics (obvious names). 
}

\source{
\enc{Väre, H., Ohtonen, R. and Oksanen, J. (1995) Effects of reindeer grazing on understorey vegetation in dry Pinus sylvestris forests. \emph{Journal of Vegetation Science} 6: 523-530.}{Vare, H., Ohtonen, R. and Oksanen, J. (1995) Effects of reindeer grazing on understorey vegetation in dry Pinus sylvestris forests. \emph{Journal of Vegetation Science} 6: 523-530.} 
}
\references{
These data are compiled form the varespec and varechem datasets in package vegan.
}
\examples{
data(cladonia)
str(cladonia)
}
\keyword{datasets}