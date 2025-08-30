\name{chalco}
\alias{chalco}
\docType{data}
\title{
Allometric Data of Giant Rhinoceros Beetles
}
\description{
Some measurements of body length and genitalia size of 156 individuals of  \emph{Chalcosoma atlas} (Linnaeus, 1758) in four localities in SW Asia.
}
\usage{data("chalco")}
\format{
  A data frame with 156 observations on the following 3 variables.
  \describe{
    \item{\code{Localidad}}{Locality. A factor with levels \code{C. Malasia}, \code{E. Tailandia}, \code{Nias}, and \code{S. Sumatra}.}
    \item{\code{Long_cuerpo}}{Body size (length) in mm.}
    \item{\code{Long_genitalia}}{Genitalia size (length) in mm.}
  }
}
\source{
Kawano, K. 2002. Character displacement in giant rhinoceros beetles. \emph{The American Naturalist} 159(3): 255-271.
}

\examples{
data(chalco)
str(chalco)
}
\keyword{datasets}
