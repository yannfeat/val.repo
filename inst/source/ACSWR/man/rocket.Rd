\name{rocket}
\alias{rocket}
\docType{data}
\title{
Rocket Propellant
}
\description{
Five different formulations of a rocket propellant x1 may be used in an aircrew escape systems on the observed burning rate Y. Here, each of the formulation is prepared by mixing from a batch of raw materials x2 which can support only five formulations required for the purpose of testing.
}
\usage{data(rocket)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{y}}{burning rate}
    \item{\code{batch}}{raw materials batch}
    \item{\code{op}}{experience of the operator}
    \item{\code{treat}}{formulation type of the propellant \code{A} \code{B} \code{C} \code{D} \code{E}}
  }
}
\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(rocket)
matrix(rocket$treat,nrow=5)
par(mfrow=c(1,3))
plot(y~factor(op)+factor(batch)+treat,rocket)
rocket_aov <- aov(y~factor(op)+factor(batch)+treat,rocket)
}
\keyword{latin square design}
