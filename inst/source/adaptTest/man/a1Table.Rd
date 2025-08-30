\name{a1Table}
\alias{a1Table}
\title{Function to produce tables of alpha1}
\description{
  This function produces tables of \eqn{\alpha_1}{alpha1} for a grid of different choices of \eqn{\alpha}{alpha} and \eqn{\alpha_0}{alpha0}.
}
\usage{
a1Table(typ, a = NA, a0 = NA, Pocock = FALSE, round = FALSE)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{a}{\emph{vector} of different choices of \eqn{\alpha}{alpha}, the overall test level}
  \item{a0}{\emph{vector} of different choices of \eqn{\alpha_0}{alpha0}, the futility stopping bound}
  \item{Pocock}{logical determining whether the "Pocock-type" should be calculated or the full level should be applied after the second stage (see details; default: full level after second stage).}
  \item{round}{rounding specification, logical or integer (see details; default: no rounding)}
}
\details{
  This function produces tables of \eqn{\alpha_1}{alpha1} on a grid spanned by the \emph{vectors} \code{a} and \code{a0} (i.e., \eqn{\alpha}{alpha} and \eqn{\alpha_0}{alpha0}). This is done either for the "Pocock-type" (i.e., under the condition \eqn{\alpha_1 = \alpha_2}{alpha1 = alpha2}: \code{Pocock = TRUE}) or using the full level after the second stage (\eqn{\alpha = \alpha_2}{alpha = alpha2}: \code{Pocock = FALSE} (the default)). The function \code{a1Table} can be a convenient shortcut for a repeated use of \code{\link{tsT}}; see this latter function for further details.

  The result is rounded to \code{round} digits after the comma (\code{round = TRUE} rounds to 1 digit; \code{round = FALSE} and \code{round = 0} prevent rounding).
}
\value{
  \code{a1Table} returns a matrix of \eqn{\alpha_1}{alpha1} values, with the corresponding \eqn{\alpha}{alpha} and \eqn{\alpha_0}{alpha0} values being displayed as dimnames.
}
\references{
Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
% \note{ }
\seealso{\code{\link{adaptTest}} package description, \code{\link{tsT}}}
\examples{
## Produce basic reference tables for the test by Vandemeulebroecke (2006)
alpha  <- c(0.1, 0.05, 0.025, 0.01)
alpha0 <- 1:10/10
a1Table(typ="v", a=alpha, a0=alpha0, Pocock=FALSE)
a1Table(typ="v", a=alpha, a0=alpha0, Pocock=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
