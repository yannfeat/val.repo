\name{CEF}
\alias{CEF}
\title{Function to specify a conditional error function}
\description{
  This function returns a conditional error function.
}
\usage{
CEF(typ = NA, fun = NA, dis = NA, a2 = NA, c = NA, p1 = NA, p2 = p1)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{c}{the parameter \eqn{c}}
  \item{a2}{\eqn{\alpha_2}{alpha2}, the local level of the test after the second stage}
  \item{p1}{the p-value \eqn{p_1}{p1} of the test after the first stage}
  \item{p2}{the p-value \eqn{p_2}{p2} of the test after the second stage, defaults to \code{p1}}
  \item{fun}{a conditional error function}
  \item{dis}{a distortion method for a supplied conditional error function (see details): \code{"pl"} for power lines, \code{"vt"} for vertical translation}
}
\details{
  There are two alternative ways of specifying the desired conditional error function:
  \itemize{
    \item through a type \code{typ}, and either a parameter (either \code{a2} or \code{c}) or a point \code{(p1,p2)}, OR
    \item through an initial conditional error function \code{fun}, and possibly a distortion method \code{dis} together with either the parameter \code{a2} or a point \code{(p1,p2)}
     }

  Most people will only need the first of these two ways; the second leads to user-defined non-standard tests.

  If \code{typ} is specified, a parameter \code{a2} or \code{c} or the point \code{(p1,p2)} must be provided. In this case, \code{CEF} returns the conditional error function of the chosen type with the given parameter or running through the given point.

  If \code{typ} is not specified, a conditional error function (i.e., a nonincreasing function defined on [0,1] with values in [0,1]) \code{fun} must be provided. If no distortion method is selected (\code{dis = NA}), \code{fun} is returned unchanged. Otherwise, the function is distorted using the chosen distortion method, either to match a desired second stage level \code{a2} or to run through a specified point \code{(p1,p2)} (one of which must be provided). Currently, two distortion methods are implemented:
  \itemize{
    \item \code{dis = "pl"}, Power lines: For an initial function \eqn{f}, define \eqn{f[r](x) = (f(x^r))^(1/r)}, r>0. Note that if \eqn{f} is a conditional error function of type \code{"b"} (Bauer and Koehne, 1994), so is \eqn{f[r]}.
    \item \code{dis = "pl"}, Vertical translation: The initial function is shifted vertically.
    }

  See \code{\link{parconv}} for more information on the two alternative parameterizations by \eqn{\alpha_2}{alpha2} and \eqn{c}.
}
\value{
  These functions return a conditional error \emph{function} (see details).
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.
  
  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
\note{
  Provide either \code{typ} or \code{fun}, not both! If \code{typ} is provided, then also specify \code{a2}, \code{c}, or \code{p1} (and possibly \code{p2}). If \code{fun} is provided, then also specify \code{dis} and \code{a2}, or \code{dis} and \code{p1} (and possibly \code{p2}), or none of these.

  Warning: Values of \code{a2} close to 0 or 1 may not work for \code{dis = "pl"}. 

  Note that in this implementation of adaptive two-stage tests, early stopping bounds are \emph{not} part of the conditional error function. Rather, they are specified separately (see also \code{\link{tsT}}).
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{parconv}}, \code{\link{plotCEF}}, \code{\link{tsT}}}
\examples{
## Plot two conditional error functions of the Lehmacher-Wassmer (1999) type:
## one to the local level alpha2=0.1, and one that runs through (p1,p2)=(0.3,0.7)
foo1 <- CEF(typ="l", a2=0.1)
foo2 <- CEF(typ="l", p1=0.3, p2=0.7)
plot(foo1, xlim=0:1)
plot(foo2, add=TRUE)

## A different way of doing the same
plotCEF(typ="l", a2=0.1, add=FALSE)
plotCEF(typ="l", p1=0.3, p2=0.7, plt.pt=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
