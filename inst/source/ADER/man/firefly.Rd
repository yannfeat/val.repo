\name{firefly}
\alias{firefly}
\docType{data}
\title{
Mating in Fireflies
}
\description{
Number of eggs laid by female fireflies (\emph{Photinus ignitus} (Fall, 1927)) after mating either with just one male or sequentially with three males. 
}
\usage{data("firefly")}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{weight}}{Weight of each female firefly.}
    \item{\code{eggs}}{Number of eggs produced after mating.}
    \item{\code{mating}}{Type of experimental mating: either  \code{single} male or \code{triple} mating with tree different males.}
  }
}
\source{
Rooney, J. and Lewis, S. M. 2002 Fitness advantage from nuptial gifts in female fireflies. \emph{Ecological Entomology} 27: 373-377.
}
\examples{
data(firefly)
str(firefly)
}
\keyword{datasets}
