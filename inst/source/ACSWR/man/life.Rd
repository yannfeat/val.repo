\name{life}
\alias{life}
\docType{data}
\title{
Life Expectancies
}
\description{
This data set consists of life expectancy in years by country, age, and sex. }
\usage{data(life)}
\format{
  A data frame with 31 observations on the following 8 variables.
  \describe{
    \item{\code{m0}}{life expectancy for males at age 0}
    \item{\code{m25}}{life expectancy for males at age 25}
    \item{\code{m50}}{life expectancy for males at age 50}
    \item{\code{m75}}{life expectancy for males at age 75}
    \item{\code{w0}}{life expectancy for females at age 0}
    \item{\code{w25}}{life expectancy for females at age 25}
    \item{\code{w50}}{life expectancy for females at age 50}
    \item{\code{w75}}{life expectancy for females at age 75}
  }
}
\references{
Everitt, B. S., and Hothorn, T. (2011). An Introduction to Applied Multivariate
Analysis with R. Springer.}
\examples{
data(life)
factanal(life,factors=1)$PVAL
factanal(life,factors=2)$PVAL
factanal(life,factors=3)
}
\keyword{factor analysis}
