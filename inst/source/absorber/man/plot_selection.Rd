\name{plot_selection}
\alias{plot_selection}
\title{Visualization of the selected variables
}
\description{
This function produces a histogram of the variable selection percentage for each variable on which \eqn{f} depends. It also displays the results obtained with the AIC.
}
\usage{
plot_selection(object)
}
\arguments{
  \item{object}{output obtained with \code{\link[=absorber]{absorber()}}.
}
}
\value{This function produces a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} plot to visualize the variables selected with \code{\link[=absorber]{absorber()}}.
}
\examples{
# --- Loading values of x --- #
data('x_obs')
# --- Loading values of the corresponding y --- #
data('y_obs')
x_trunc = x_obs[1:70,,drop=FALSE]
y_trunc = y_obs[1:70]

# --- Variable selection of f1 --- #
res = absorber(x=x_trunc, y=y_trunc, M = 3)
plot_selection(res)
}