\name{RIKZ}
\alias{RIKZ}
\docType{data}
\title{
RIKZ Data
}
\description{
Richness of benthic species in 45 sampling stations along the coastline of The Netherlands, measured by two researches of the the RIKZ (Rijksinstituut voor Kust en Zee), i.e., the Dutch Institute for Coastal and Marine Management. 
}
\usage{data("RIKZ")}
\format{
  A data frame with  the abundance (number of individuals) of 75 benthic macrofauna species (columns 2 -\code{C1}- to 76 -\code{I5}-) and several environmental meassurements recorded in each sampling station (\code{Sample}), i.e., the five sampling stations within each of the nine beaches considered (\code{Beach}). The most important is NAP, i.e., Normal Amsterdams Peil (\code{NAP}), which measures the height of the sampling station (in m) in relation to the average sea level. Other variables are related to the nature of the sandy sediment, i.e.,  average grain size (\code{grainsize}), grain sorting (\code{sorting1}), penetrability (\code{penetrability}), carbonate content (\code{chalk}), or to each beach, i.e., slope (\code{angle2}), salinity (\code{salinity}) and average temperature (\code{temperature}).
}
\source{
Janssen, G. and Mulder, S.. 2005. Zonation of macrofauna across sandy beaches and surf zones along the Dutch coast. \emph{Oceanologia} 47(2): 265-282.
}
\examples{
data(RIKZ)
str(RIKZ)
}
\keyword{datasets}
