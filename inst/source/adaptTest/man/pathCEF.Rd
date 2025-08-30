\name{pathCEF}
\alias{pathCEF}
\title{Function to plot several conditional error functions running through a "path" of given points}
\description{
  This function plots several conditional error functions of the same family such that each one runs through one of several given points.
}
\usage{
pathCEF(typ = NA, fun = NA, dis = NA, p1 = 1:49/50, p2 = p1,
        x = 0:200/200, plt.pt = FALSE, plt.ptann = FALSE, xlab = NA, ylab = NA, ...)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{fun}{a conditional error function}
  \item{dis}{a distortion method for a supplied conditional error function (see details): \code{"pl"} for power lines, \code{"vt"} for vertical translation}
  \item{p1}{a vector (at least of length 2) of p-values \eqn{p_1}{p1} of the test after the first stage}
  \item{p2}{a vector (at least of length 2) of p-values \eqn{p_2}{p2} of the test after the second stage, defaults to \code{p1}; must be of same length as \code{p1}}
  \item{x}{vector on which the conditional error functions are plotted (should be relatively dense in [0,1])}
  \item{plt.pt}{logical determining whether the points that the conditional error functions are made to run through should be plotted or not (default: not)}
  \item{plt.ptann}{logical determining whether the points that the conditional error functions are made to run through should be annotated or not (default: not)}
  \item{xlab}{a label for the x axis (default: no label)}
  \item{ylab}{a label for the y axis (default: no label)}
  \item{\dots}{arguments to be passed on to the underlying \code{plot} and \code{points} functions (e.g., graphical parameters)}
}
\details{
  It can be instructive to plot not only one conditional error function, but to visualize a whole family. This can easily be done with \code{pathCEF}. The function is used in a similar way as \code{\link{plotCEF}}, but \code{p1} and \code{p2} are now vectors (of the same length, at least of length 2). Conditional error functions are plotted that run through the specified elementwise points \code{(p1,p2)} (which by default lie on the main diagonal).

  Internally, \code{pathCEF} uses \code{\link{plotCEF}} to plot the individual conditional error functions; see this latter function for further details.
}
\value{
  The function \code{pathCEF} is invoked for its plotting effect; it returns no meaningful value.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
\note{
  Provide either \code{typ} or \code{fun}, not both! If \code{fun} is provided, then also specify \code{dis}.

  Unlike \code{plotCEF}, it is not possible with \code{pathCEF} to specify the conditional error functions by the parameter \eqn{\alpha_2}{alpha2} or the parameter \eqn{c}.

  \code{plt.ptann} is not considered if \code{plt.pt = FALSE}.
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{CEF}}, \code{\link{plotCEF}}, \code{\link{tsT}}}
\examples{
## Compare the tests by Bauer and Koehne (1994),
##  Lehmacher and Wassmer (1999) and Vandemeulebroecke (2006)
oldmfcol <- par(mfcol=c(1,3))
pathCEF(typ="b", main="BK 94")
pathCEF(typ="l", main="LW 99")
pathCEF(typ="v", main="V 06")
par(oldmfcol)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
