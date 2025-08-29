\name{abrasion_index}
\alias{abrasion_index}
\docType{data}
\title{
Abrasion Index for the Tire Tread
}
\description{
To understand the relationship between the abrasion index for the tire tread, the output y, as a linear function of the hydrated silica level x1, silane coupling agent level x2 and the sulfur level x3, Derringer and Suich (1980) collected data on 14 observation points. 
}
\usage{data("abrasion_index")}
\format{
  A data frame with 14 observations on the following 4 variables.
  \describe{
    \item{\code{x1}}{hydrated silica level}
    \item{\code{x2}}{silane coupling agent level}
    \item{\code{x3}}{sulfur level}
    \item{\code{y}}{abrasion index for the tire tread}
  }
}
\references{
Derringer, G., and Suich, R. (1980). Simultaneous Optimization of Several Response
Variables. Journal of Quality Technology, 12, 214-219.
}
\examples{
data(abrasion_index)
ailm <- lm(y~x1+x2+x3,data=abrasion_index)
pairs(abrasion_index)
}
\keyword{datasets}
