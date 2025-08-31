\name{QRS}
\alias{QRS}
\title{QR Regression Selection and Estimation}
\description{Given a design matrix and a response variable, create a list which has the fitted model, estimated regression coefficents and standard error based on interrupted coefficient estimation selection.}

\usage{QRS(x, y, Nsims)}
\arguments{
  \item{x}{a numeric matrix; usually the model matrix for a multiple regression model.}
  \item{y}{a numeric vector; usually the values of the response variable in the regression model.}
  \item{Nsims}{number of simulation runs required for estimating the regression coefficient standard errors.}
}
\value{
a QRS class object
 \item{coefficients}{a named numeric vector of coefficients}
 \item{residuals}{a numeric vector containing the response minus the fitted values. }
 \item{effects}{a numeric vector of containing the projections of the response variable under the orthogonal
Q matrix coming from the QR decomposition of the model matrix.} 
 \item{rank}{ the numeric rank of the fitted linear model. }
 \item{fitted.values}{the estimated response values according to the fitted interrupted coefficient estimation
selection regression model.}
 \item{sigma2}{the estimated noise variance based on the n-p residual effects, where p is the size of the full model.}
 \item{std_error}{a numeric vector of standard errors.}
 \item{qr}{the QR decomposition object coming from the model matrix (after re-ordering columns).}
 \item{df.residual}{ he residual degrees of freedom.}
 \item{model}{if requested (the default), the model frame used.}
 \item{x}{a numeric matrix containing the model matrix.}
 \item{y}{a numeric vector containing the response variable values.}
 \item{coefOrder}{A permutation of the sequence 1:p which gives the 
ascending order of the coefficients of the linear model object, as a result of the pre-screening.}
 \item{names}{a character vector containing the column names of the model matrix.}
}
\details{
The interrupted coefficient estimation selection
 procedure begins with consideration of a full model whereby a regression model with p terms is fit to
n observations on a response and p-1 predictor variables.  The variables are pre-screened by application
of lm in order to cast the columns of the model matrix in increasing order of the p-values observed
for the corresponding regression coefficients.    The estimation then proceeds by the usual QR decomposition
of the model matrix but is interrupted at the effects stage.  The effects are classified as "different from 0"
or "not different from 0", according to what is essentially a control chart procedure. The effects that are "not
different from 0" are replaced with true 0's and the nonzero effects are left alone. The estimation is completed
by backward-substitution solution of the zero and nonzero effects using the upper triangular matrix from
the QR decomposition.  The result is a set of coefficient estimates that will tend to be more accurate
in a mean-squared-error sense than the original lm coefficient estimates, especially when some or all
of the regression coefficients are 0. Coefficient standard error estimates are obtained by a parametric
bootstrap procedure.  This method is not recommended for strongly non-normal data, or where there is 
substantial multicollinearity.}

\author{Ladan Tazik, W.J. Braun}
\seealso{
\code{ices.R}, \code{lm.R}
}
\keyword{models}
