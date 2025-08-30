\name{getpar}
\alias{getpar}
\title{Function to calculate the parameter that specifies the conditional error function running through a given point}
\description{
  This function calculates the parameter that specifies the conditional error function running through a given point \eqn{(p_1,p_2)}{(p1,p2)}, based on a chosen family of conditional error functions.
}
\usage{
getpar(typ, p1 = NA, p2 = p1, c = FALSE)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{p1}{the p-value \eqn{p_1}{p1} of the test after the first stage}
  \item{p2}{the p-value \eqn{p_2}{p2} of the test after the second stage, defaults to \code{p1}}
  \item{c}{logical determining whether the parameter \eqn{\alpha_2}{alpha2} or the parameter \eqn{c} is returned (\eqn{\alpha_2}{alpha2} is the default).}
}
\details{
  See \code{\link{parconv}} for more information on the two alternative parameterizations by \eqn{\alpha_2}{alpha2} and \eqn{c}.
}
\value{
  \code{getpar} returns the parameter (either \eqn{\alpha_2}{alpha2} or \eqn{c}, depending on the chosen parameterization) that specifies the conditional error function running through \eqn{(p_1,p_2)}{(p1,p2)}.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
% \note{ }
\seealso{\code{\link{adaptTest}} package description, \code{\link{parconv}}, \code{\link{CEF}}}
\examples{
## Plot the conditional error function of the Lehmacher-Wassmer (1999)
##  type that runs through (p1,p2)=(0.3,0.7)
alpha2 <- getpar(typ="l", p1=0.3, p2=0.7)
plotCEF(typ="l", a2=alpha2, add=FALSE)

## Other ways of doing the same as above
plotCEF(typ="l", p1=0.3, p2=0.7, add=FALSE)
plot(CEF(typ="l", p1=0.3, p2=0.7), xlim=0:1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
