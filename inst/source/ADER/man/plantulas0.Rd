\name{plantulas0}
\alias{plantulas0}
\docType{data}
\title{
Monitoring of Helianthemum Seedlings
}
\description{
Monitoring of several cohorts of seedlings of \emph{Helianthemum squamatum} (L.) Dum. Cours., from emergence to the end of sumer drought.
}
\usage{data("plantulas0")}
\format{
  A data frame with 691 observations on the following 6 variables.  \describe{
    \item{\code{Censo}}{Cohort, i.e., when the seedling was first censused. A factor with levels \code{abril} (April), \code{junio} (June), \code{marzo} (March), \code{mayo} (May).}
    \item{\code{MSET}}{Fate of each seedling at September census. A factor with levels \code{herbivoria} (died by herbivory), \code{sequia} (died by drought), \code{sobrevive} (survived).}
    \item{\code{Hjun}}{Number of leaves in the June census.}
    \item{\code{Tjun}}{Height of the seedling (cm) in the June census.}
    \item{\code{NN05}}{Number of neighbour seedlings within a circle with a radius of 5 cm around each seedling.}
    \item{\code{NN10}}{Number of neighbour seedlings within a circle with a radius of 10 cm  around each seedling.}
  }
}
\references{
De la Cruz, M., Romao, R.L., Escudero, A. and Maestre, F.T. 2008. Where do seedlings go? A spatio-temporal analysis of seedling mortality in a semi-arid gypsophyte. \emph{Ecography} 31:  720-730 .
}
\examples{
data(plantulas0)
str(plantulas0)
}
\keyword{datasets}
