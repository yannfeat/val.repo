\name{arrecifes}
\alias{arrecifes}
\docType{data}
\title{
Californian Reefs
}
\description{
Data about competition between seaweeds and sessile benthic invertebrates in three Californian reefs. Data were collected in sixteen 40 m x 2 m transects during 8 years. This dataset includes the "non-urchin barren" (transects where the density of sea urchins was low, which includes 4 transects x 7 yr + 6 transects x 6 yr +1 transect x 5 yr).
}
\usage{data("arrecifes")}
\format{
  A data frame with 69 observations on the following 3 variables.
  \describe{
    \item{\code{sitio}}{Location of the transect. A factor with three levels: \code{Carpinteria}, \code{Mohawk} and \code{Naples}.}
    \item{\code{algas}}{Percentage cover of seaweds.}
    \item{\code{invertebrados}}{Percentage cover of sessile invertebrates.}
  }
}
\details{
These data have been digitized from Figure 3a of Arkema \emph{et al.} 2009.
}
\source{
Arkema, K,K., Reed, D.C., and Schroether, S.C. 2009.  Direct and indirect effects of giant kelp determine benthic community structure and dynamics. \emph{Ecology} 90: 3126-3137.
}
\examples{
data(arrecifes)
str(arrecifes) 
}
\keyword{datasets}
