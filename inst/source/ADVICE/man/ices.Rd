\name{ices}
\alias{ices}
\title{Interrupted Coefficient Estimation Selection}
\description{This function provides an alternative multiple regression fitting procedure
which simultaneously estimates and selects variables.  The resulting coefficient estimates
will tend to be slightly biased, but in a sparse setting, they can be quite accurate. A
full regression model is specified by the user, and the function usually returns coefficient estimates
for a reduced model, i.e., a model for which some of the coefficient estimates are exactly 0.   
}
\usage{
    ices(formula, data, model = TRUE, x = FALSE, y = FALSE, qr = TRUE)
}
\arguments{
  \item{formula}{a formula object specifying the full regression model.}
  \item{data}{a data frame containing observations on the response variable and the predictor variables.}
  \item{model, x, y, qr}{logicals.  If \code{TRUE} the corresponding components of
          the fit (the model frame, the model matrix, the response, the
          QR decomposition) are returned.}
}
\value{
a QRS class object
 \item{coefficients}{a named numeric vector of coefficients}
 \item{residuals}{a numeric vector containing the response minus the fitted values. }
 \item{effects}{a numeric vector of containing the projections of the response variable under the orthogonal
Q matrix coming from the QR decomposition of the model matrix.} 
 \item{rank}{the numeric rank of the fitted linear model. }
 \item{fitted.values}{the estimated response values according to the fitted interrupted coefficient estimation
selection regression model.}
 \item{sigma2}{the estimated noise variance based on the n-p residual effects, where p is the size of the full model.}
 \item{std_error}{a numeric vector of standard errors.}
 \item{df.residual}{residual degrees of freedom.}
 \item{x}{a numeric matrix containing the model matrix.}
 \item{y}{a numeric vector containing the response variable values.}
 \item{qr}{the QR decomposition object coming from the model matrix (after re-ordering columns).}
 \item{coefOrder}{permutation of the sequence 1:p which gives the 
ascending order of the coefficients of the linear model object, as a result of the pre-screening.}
 \item{call}{the matched call.}
 \item{terms}{the terms object used.}
 \item{names}{a character vector containing the column names of the model matrix.}
\item{model}{if requested (the default), the model frame used in the case of the full regression model.}
}
\author{Ladan Tazik, W.J. Braun}
\seealso{
\code{lm.R}, \code{QRS.R}
}
\examples{
    myRegressionData <- rmultreg(50, k=10, p=.25, sdnoise = .5)
    pairs(myRegressionData$data)
    out <- ices(y ~ ., data = myRegressionData$data) # fit model to simulated data
    confint(out) # calculate 95 % confidence intervals for all coefficients
    myRegressionData$coefficients # compare with true coefficients
}
\keyword{models}
