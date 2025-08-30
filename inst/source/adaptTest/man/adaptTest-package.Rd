\name{adaptTest-package}
\alias{adaptTest-package}
\alias{adaptTest}
\docType{package}
\title{
Adaptive two-stage tests
}
\description{
The functions defined in this program serve for implementing adaptive two-stage tests.
}
\details{
\tabular{ll}{
Package: \tab adaptTest\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2023-10-05\cr
License: \tab GPL (version 2 or later)\cr
LazyLoad: \tab yes\cr
}
An adaptive two-stage test can be considered as a family of decreasing functions \eqn{f[c](p_1)}{f[c](p1)} in the unit square. Each of these functions is a conditional error function, specifying the type I error conditional on the p-value \eqn{p_1}{p1} of the first stage. For example, \eqn{f[c](p_1) = \min(1, c/p_1)}{f[c](p1) = min(1, c/p1)} corresponds to Fisher's combination test (Bauer and Koehne, 1994). Based on this function family, the test can be put into practice by specifying the desired overall level \eqn{\alpha}{alpha}, stopping bounds \eqn{\alpha_1 <= \alpha_0}{alpha1 <= alpha0} and a parameter \eqn{\alpha_2}{alpha2}. After computing \eqn{p_1}{p1}, the test stops with or without rejection of the null hypothesis if \eqn{p_1 <= \alpha_1}{p1 <= alpha1} or \eqn{p_1 > \alpha_0}{p1 > alpha0}, respectively. Otherwise, the null hypothesis is rejected if and only if \eqn{p_2 <= f[c](p_1)}{p2 <= f[c](p1)} holds for the p-value \eqn{p_2}{p2} of the second stage, where \eqn{c} is such that the local level of this latter test is \eqn{\alpha_2}{alpha2} (e.g., \eqn{c = c(\alpha_2) = \exp(-\chi^2_{4,\alpha_2}/2)}{c = c(alpha2) = exp(-chi2_{4,alpha2}/2)} for Fisher's combination test).

This package provides functions for handling conditional error functions, performing calculations among the different parameters (\eqn{\alpha}{alpha}, \eqn{\alpha_0}{alpha0}, \eqn{\alpha_1}{alpha1}, \eqn{\alpha_2}{alpha2} and \eqn{c}) and computing overall p-values, in addition to graphical visualization routines. Currently, four predefined tests are included: Bauer and Koehne (1994), Lehmacher and Wassmer (1999), Vandemeulebroecke (2006), and the horizontal conditional error function. User-defined tests can also be implemented.

This package contains the following functions:
\itemize{
  \item Key functions are \code{\link{CEF}}, \code{\link{plotCEF}}, \code{\link{tsT}}, \code{\link{ovP}}.
  \item Further functions are \code{\link{a1Table}}, \code{\link{getpar}}, \code{\link{parconv}}, \code{\link{pathCEF}}, \code{\link{plotBounds}}, \code{\link{eq}}, \code{\link{ne}}, \code{\link{ge}}, \code{\link{gt}}, \code{\link{le}}, \code{\link{lt}}.
  }
The functions \code{\link{a1Table}}, \code{\link{getpar}}, \code{\link{parconv}} and \code{\link{tsT}} can handle the four predefined tests mentioned above. The functions \code{\link{CEF}}, \code{\link{plotCEF}}, \code{\link{pathCEF}} and \code{\link{ovP}} can also handle these, and user-defined tests in addition. The functions \code{\link{plotBounds}}, \code{\link{eq}}, \code{\link{ne}}, \code{\link{ge}}, \code{\link{gt}}, \code{\link{le}} and \code{\link{lt}} do not directly handle tests.
}
\author{
Marc Vandemeulebroecke

Maintainer: Marc Vandemeulebroecke <vandemem(at)gmx.de>
}
\note{
  Note that a family of conditional error functions can be parameterized in two alternative ways: more "traditionally" by some parameter \eqn{c} that, in turn, depends on the local level \eqn{\alpha_2}{alpha2} of the test after the second stage, or - perhaps more conveniently - by \eqn{\alpha_2}{alpha2} itself.

  In this implementation, early stopping bounds are \emph{not} part of the conditional error function. Rather, they are specified separately and "imposed" on it.

  I want to thank Niklas Hack for technical support.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Brannath, W., Posch, M., Bauer, P. (2002). Recursive combination tests. \emph{J. Amer. Statist. Assoc.} 97, 236-244.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.

  Vandemeulebroecke, M. (2006). \emph{A general approach to two-stage tests.} Doctoral thesis, Otto-von-Guericke-Universitaet Magdeburg, \code{http://www.dissertation.de}.

  Vandemeulebroecke, M. (2008). Group sequential and adaptive designs - a review of basic concepts and points of discussion. \emph{Biometrical Journal} 50, 541-557.
}
\keyword{ package }
\seealso{\code{\link{CEF}}, \code{\link{tsT}}}
\examples{
## Example from Bauer and Koehne (1994)
alpha  <- 0.1
alpha2 <- 0.1
alpha0 <- 0.5
alpha1 <- tsT(typ="b", a=alpha, a0=alpha0, a2=alpha2)
plotCEF(typ="b", a2=alpha2, add=FALSE)
plotBounds(alpha1, alpha0)
CEF(typ="b", a2=alpha2)
}
