\name{viscos}
\alias{viscos}
\docType{data}
\title{
The Box-Cox Transformation for Viscosity Dataset
}
\description{
The goal of this study is to find the impact of temperature on the viscosity of toluence-tetralin blends.
}
\usage{data(viscos)}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{Temperature}}{temperature }
    \item{\code{Viscosity}}{viscosity of toluence-tetralin blends}
  }
}
\references{
Montgomery, D.C., Peck, E.A., and Vining, G.G. (1983-2012). Introduction to Linear Regression Analysis, 5e. J. Wiley. 
}
\examples{
data(viscos)
names(viscos)
viscoslm <- lm(Viscosity~Temperature,data=viscos)
}
\keyword{Box-Cox transformation}