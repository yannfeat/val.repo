\name{pw}
\alias{pw}
\docType{data}
\title{
The Linguistic Probe Word Analysis
}
\description{
Probe words are used to test the recall ability of words in various linguistic contexts. In this experiment the response time to five different probe words are recorded for 11 individuals. The interest in the experiment is to examine if the response times to the different words are independent or not.
}
\usage{data(pw)}
\format{
  A data frame with 11 observations on the following 6 variables.
  \describe{
    \item{\code{Subject.Number}}{a numeric vector}
    \item{\code{y1}}{a numeric vector}
    \item{\code{y2}}{a numeric vector}
    \item{\code{y3}}{a numeric vector}
    \item{\code{y4}}{a numeric vector}
    \item{\code{y5}}{a numeric vector}
  }
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.
}
\examples{
data(pw)
sigma <- var(pw[2:6])
p <- ncol(pw)-1; v <- nrow(pw)-1
u <- p^p*(det(sigma))/(sum(diag(sigma))^p)
u1 <- -(v-(2*p^2+p+2)/(6*p))*log(u)
u;u1
}
\keyword{Testing for sphericity}
