\name{rocket_Graeco}
\alias{rocket_Graeco}
\docType{data}
\title{
Rocket Propellant Example Extended}
\description{
In continuation of Example 13.4.7 of the Rocket Propellant data, we now have the added blocking factor in test assemblies.
}
\usage{data(rocket_Graeco)}
\format{
  A data frame with 25 observations on the following 5 variables.
  \describe{
    \item{\code{y}}{burning rate}
    \item{\code{batch}}{raw materials batch}
    \item{\code{op}}{experience of the operator}
    \item{\code{treat}}{formulation type of the propellant \code{A} \code{B} \code{C} \code{D} \code{E}}
    \item{\code{assembly}}{test assemblies \code{a} \code{b} \code{c} \code{d} \code{e}}
  }
}
\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(rocket_Graeco)
plot(y~op+batch+treat+assembly,rocket_Graeco)
rocket.glsd.aov <- aov(y~factor(op)+factor(batch)+treat+assembly,rocket_Graeco)
summary(rocket.glsd.aov)
}
\keyword{Graeco Latin square design}
