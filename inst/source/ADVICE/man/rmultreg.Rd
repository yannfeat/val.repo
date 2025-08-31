\name{rmultreg}
\alias{rmultreg}
\title{
Multiple Regression Data Generator
}
\description{
Values of any number of predictor variables and a single response variable 
are simulated according to a model with randomly generated coefficients. 
Values of each predictor are simulated independently from standard normal 
distributions.  The regression coefficients are generated independently
from a uniform distribution on the interval (minimum, maximum), and each
coefficient is multiplied by a Bernoulli (p) variate, independent of 
the other coefficients.  This results in some of the coefficients being
zeroed out.  Noise is added to the regression response according to
independent t variates with degrees of freedom equal to dfnoise. 
}
\usage{
rmultreg(n, k = 1, minimum = 0, maximum = 1, p = 0.5, dfnoise = 100, sdnoise = 1) 
}
\arguments{
  \item{n}{number of observations.}
  \item{k}{number of predictor variables in addition to the intercept.}
  \item{minimum}{minimum possible value for the regression coefficients, apart, possibly, from some zeroes.}
  \item{maximum}{maximum possible value for the regression coefficients, apart, possibly, from some zeroes.}
  \item{p}{probability that a given regression coefficient remains nonzero.}
  \item{dfnoise}{degrees of freedom for t-distributed additive noise.}
  \item{sdnoise}{standard deviation of the noise term.}
}
\value{a list containing
  \item{data}{a dataframe containing n observations on k predictor variables and a response y.}
  \item{coefficients}{a numeric vector containing the true regression coefficients.}
}
\author{
W.J. Braun
}
\examples{
    myRegressionData <- rmultreg(50, k=3, p=.5, sdnoise = .25)
    pairs(myRegressionData$data)
    out <- ices(y ~ ., data = myRegressionData$data) # fit model to simulated data
    confint(out) # calculate 95% confidence intervals for all coefficients
    myRegressionData$coefficients # compare with true coefficients
}
\keyword{datagen}
