\name{absorber}
\alias{absorber}
\title{Variable selection in nonparametric models
}
\description{
This function implements the method described in Savino, M. E. and Levy-Leduc, C (2024) for variable selection in nonlinear multivariate settings where observations are assumed to satisfy a nonparametric regression model. Each observation point should belong to \eqn{[0,1]^p}.  
}
\usage{
absorber(x, y, M = 3, K = 1, all.variables = NULL, parallel = FALSE, nbCore = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix of \eqn{p} columns containing the input values of the observations, each observation belonging to \eqn{[0,1]^p}.
}
  \item{y}{
  vector containing the corresponding response variable associated to the input values \eqn{\texttt{x}}.
}
  \item{M}{
order of the B-spline basis used in the regression model. Default is 3 (quadratic B-splines).
}
  \item{K}{
  number of evenly spaced knots to use in the B-spline basis. Default value is 1.
}
  \item{all.variables}{
list of characters or integers, labels of the variables. Default is \eqn{\texttt{NULL}}. 
}
 \item{parallel}{
  logical, if TRUE then a parallelized version of the code is used. Default is FALSE. 
}
  \item{nbCore}{
 numerical, number of cores used for parallelization, if parallel is set to TRUE.
}
}
\value{
\item{selec.var}{list of vectors of the selected variables, one vector for each penalization parameter.}
\item{aic.var}{vector of variables selected using AIC.}
}
\examples{
# --- Loading values of x --- #
data('x_obs')
# --- Loading values of the corresponding y --- #
data('y_obs')
x_trunc = x_obs[1:70,,drop=FALSE]
y_trunc = y_obs[1:70]

# --- Variable selection of f1 --- #
absorber(x=x_trunc, y=y_trunc, M = 3)
\donttest{
# --- Parallel computing --- #
absorber(x=x_trunc, y=y_trunc, M = 3, parallel = TRUE, nbCore = 2)
} 
}
