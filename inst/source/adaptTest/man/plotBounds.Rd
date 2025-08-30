\name{plotBounds}
\alias{plotBounds}
\title{Function to plot the stopping bounds of an adaptive two-stage test}
\description{
  This function plots the stopping bounds of an adaptive two-stage test.
}
\usage{
plotBounds(a1 = 0, a0 = 1, add = TRUE, xlab = NA, ylab = NA, ...)
}
\arguments{
  \item{a1}{\eqn{\alpha_1}{alpha1}, the efficacy stopping bound and local level of the test after the first stage (default: no stopping for efficacy)}
  \item{a0}{\eqn{\alpha_0}{alpha0}, the futility stopping bound (default: no stopping for futility)}
  \item{add}{logical determining whether the bounds should be added to an existing plot (default) or a new plot should be opened}
  \item{xlab}{a label for the x axis (default: no label)}
  \item{ylab}{a label for the y axis (default: no label)}
  \item{\dots}{arguments to be passed on to the underlying \code{lines} functions (e.g., graphical parameters)}
}
\details{
  This function plots the stopping bounds \eqn{\alpha_1}{alpha1} and \eqn{\alpha_0}{alpha0} of an adaptive two-stage test, either onto an existing plot or into a new plot.
}
\value{
  The function \code{plotBounds} is invoked for its plotting effect; it returns no meaningful value.
}
\author{Marc Vandemeulebroecke}
\note{
  Note that in this implementation of adaptive two-stage tests, early stopping bounds are \emph{not} part of the conditional error function. Rather, they are specified separately (see also \code{\link{tsT}}).
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{plotCEF}}}
\examples{
## Example from Bauer and Koehne (1994): full level after final stage, alpha0 = 0.5
alpha  <- 0.1
alpha2 <- 0.1
alpha0 <- 0.5
alpha1 <- tsT(typ="b", a=alpha, a0=alpha0, a2=alpha2)
plotCEF(typ="b", a2=alpha2, add=FALSE)
plotBounds(alpha1, alpha0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
