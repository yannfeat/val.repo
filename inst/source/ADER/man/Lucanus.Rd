\name{Lucanus}
\alias{Lucanus}
\docType{data}
\title{
Allometry Data of  Stag Beetles
}
\description{
Measurements of head width and elytra length of several individuals of \emph{Lucanus cervus} (Linnaeus, 1758) recorded in several localities in Northern Spain.
}
\usage{data("Lucanus")}
\format{
  A data frame with 265 observations on the following 4 variables.
  \describe{
    \item{\code{KB}}{Head size (mm).}
    \item{\code{EL}}{Elytra length (mm).}
    \item{\code{SEXO}}{Sex, a factor with levels \code{hembra} (female) or  \code{macho} (male).}
    \item{\code{PROVINCIA}}{Spanish province where each individual was collected. Either \code{Asturias}, \code{Cantabria} or \code{Otras} (other).}
  }
}
\examples{
data(Lucanus)
str(Lucanus)
}
\keyword{datasets}
