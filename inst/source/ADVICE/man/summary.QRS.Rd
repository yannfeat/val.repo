\name{summary.QRS}
\alias{summary.QRS}
\encoding{UTF-8}
\title{summary.QRS}
\description{summary method for class "qrs"}
\usage{
\method{summary}{QRS}(object, ...)
}
\arguments{
  \item{object}{ an abject of class "qrs"}
  \item{...}{additional arguments affecting the summary produced.}
}
\value{
The function computes and returns a list of summary statistics of the fitted linear model given in the  QRS object.
\item{Residuals}{the weighted residuals, the usual residuals rescaled by the square root of the weights specified in the call to qrs}
\item{Coefficients}{a p x 4 matrix with columns for the estimated coefficient, its standard error, z-score and corresponding (two-sided) probabilities}
\item{df}{degrees of freedom}
\item{residualStandardError}{Residual standard error}
}
\author{Ladan Tazik, W.J.Braun}
\seealso{
\code{QRS.R}
}
\examples{
    myRegressionData <- rmultreg(25, k=5, p=.15, sdnoise = .25)
    pairs(myRegressionData$data)
    out <- ices(y ~ ., data = myRegressionData$data) # fit model to simulated data
    summary(out) # estimates and standard errors for all coefficients
    myRegressionData$coefficients # compare with true coefficients
}


\keyword{models}
