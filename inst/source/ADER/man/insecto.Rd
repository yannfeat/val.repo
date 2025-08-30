\name{insecto}
\alias{insecto}
\docType{data}
\title{
Insects and Plant Disease
}
\description{  
Observations of the presence of a plant disease and of a certain species of insect in 14 plants.
}
\usage{data("insecto")}
\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{enfermedad}}{Presence of the plant disease. A factor with levels \code{ausente}  (absent) and \code{presente} (present).}
    \item{\code{insecto}}{Presence of the insect species. A factor with levels \code{ausente}  (absent) and \code{presente} (present).}
  }
}

\source{
Zar, J. H. 2010. \emph{Biostatistical Analysis}. 5th ed. Prentice Hall.
}
\examples{
data(insecto)
str(insecto)
}
\keyword{datasets}
