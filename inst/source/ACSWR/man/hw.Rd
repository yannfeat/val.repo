\name{hw}
\alias{hw}
\docType{data}
\title{
Height-Weight Covariance Study}
\description{
The data set highlights the importance of handling covariance when such information is available. If the covariance is not incorporated, hypothesis testing may lead to entirely difference conclusion. }
\usage{data(hw)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Height}}{the height of an individual}
    \item{\code{Weight}}{the weight of an individual}
  }
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.}
\examples{
data(hw)
sigma0 <- matrix(c(20, 100, 100, 1000),nrow=2)
sigma <- var(hw)
v <- nrow(hw)-1
p <- ncol(hw)
u <- v*(log(det(sigma0))-log(det(sigma)) + sum(diag(sigma\%*\%solve(sigma0)))-p)
u1 <- (1- (1/(6*v-1))*(2*p+1 - 2/(p+1)))*u
u;u1;qchisq(1-0.05,p*(p+1)/2)
}
\keyword{testing mean vector}
