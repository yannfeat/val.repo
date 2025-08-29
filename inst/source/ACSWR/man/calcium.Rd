\name{calcium}
\alias{calcium}
\docType{data}
\title{
The Calcium in Soil}
\description{
Kramer and Jensen (1969) collected data on three variables at 10 different locations. The variables of interest are available calcium in the soil, y1, exchangeable soil calcium, y2, and turnip green calcium, y3. The hypothesis of interest is whether the mean vector is [15.0 6.0 2.85].}
\usage{data(calcium)}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{Location.Number}}{a numeric vector}
    \item{\code{y1}}{a numeric vector}
    \item{\code{y2}}{a numeric vector}
    \item{\code{y3}}{a numeric vector}
  }
}
\source{
Kramer, C. Y., and Jensen, D. R. (1969). Fundamentals of Multivariate Analysis, Part I. Inference about Means. Journal of Quality Technology, 1 (2), 120-133.}
\references{
Rencher, A.C. (1990-2002). Methods of Multivariate Analysis, 2e. J. Wiley.
}
\examples{
data(calcium)
n <- nrow(calcium)
meanx <- colMeans(calcium[,-1])
varx <- var(calcium[,-1])
mu0 <- c(15,6,2.85)
t2 <- n*t(meanx-mu0)%*%solve(varx)%*%(meanx-mu0)
t2
}
\keyword{multivariate hypothesis testing}
