\name{confint.QRS}
\alias{confint.QRS}
\title{Confidence Interval Function}
\description{Computes confidence intervals for one or more parameters in a fitted model. There is a default and a method for objects inheriting from class "qrs".}
\usage{
\method{confint}{QRS}(object, parm, level, ...)
}
\arguments{
  \item{object}{a fitted model object from the QRS class.}
  \item{parm}{a specification of which parameters are to be given
          confidence intervals, either a vector of numbers or a vector
          of names.  If missing, all parameters are considered.}
  \item{level}{a numeric value specifying the required confidence level.}
  \item{...}{additional argument(s) for the methods.}
}
\details{This function computes t-based confidence intervals using n-p degrees of freedom, where n is the number of observations and p is the number of regression coefficients in the full model.}
\value{
   A 2-column matrix giving lower and upper confidence limits (corresponding to the given level) 
   for each parameter. These will be labelled as (1-level)/2 and 1 - (1-level)/2 in % (by default 2.5% and 97.5%).
}
\author{Ladan Tazik, W.J. Braun}
\seealso{
\code{ices.R}
}
\examples{
    myRegressionData <- rmultreg(100, k=20, p=.1, sdnoise = 1)
    pairs(myRegressionData$data)
    out <- ices(y ~ ., data = myRegressionData$data) # fit model to simulated data
    confint(out) # calculate 95% confidence intervals for all coefficients
    myRegressionData$coefficients # compare with true coefficients
}
\keyword{models}
