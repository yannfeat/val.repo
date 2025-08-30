\name{ovP}
\alias{ovP}
\title{Function to compute and visualize overall p-values}
\description{
  This function computes and plots overall p-values for adaptive two-stage tests.
}
\usage{
ovP(typ = NA, fun = NA, dis = NA, p1 = 1:49/50, p2 = p1,
    a1 = 0, a0 = 1, grid = FALSE, plt = FALSE,
    invisible = FALSE, wire = FALSE, round = FALSE)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{fun}{a conditional error function}
  \item{dis}{a distortion method for a supplied conditional error function (see details): \code{"pl"} for power lines, \code{"vt"} for vertical translation}
  \item{p1}{the p-value \eqn{p_1}{p1} of the test after the first stage, or a vector of such p-values}
  \item{p2}{the p-value \eqn{p_2}{p2} of the test after the second stage, or a vector of such p-values; defaults to \code{p1}}
  \item{a1}{\eqn{\alpha_1}{alpha1}, the efficacy stopping bound and local level of the test after the first stage (default: no stopping for efficacy)}
  \item{a0}{\eqn{\alpha_0}{alpha0}, the futility stopping bound (default: no stopping for futility)}
  \item{grid}{logical determining whether a grid should be spanned by \code{p1} and \code{p2} (default: no grid is spanned)}
  \item{plt}{logical determining whether the overall p-values should be plotted or not (default: not)}
  \item{invisible}{logical determining whether the printing of the overall p-values should be suppressed or not (default: not)}
  \item{wire}{logical determining whether the overall p-values should be plotted in wireframe-style or in cloud-style (default: cloud-style)}
  \item{round}{rounding specification, logical or integer (see details; default: no rounding)}
}
\details{
  The overall p-value for an adaptive two-stage test is computed as \eqn{p_1}{p1} if \eqn{p_1 <= \alpha_1}{p1 <= alpha1} or \eqn{p_1 > \alpha_0}{p1 > alpha0}, and as \deqn{\alpha_1 + \int_{\alpha_1}^{\alpha_0} cef_{(p_1,p_2)}(x) d x}{alpha1 + int_{alpha1}^{alpha0} cef_{(p1,p2)}(x) d x} otherwise, where \eqn{cef_{(p_1,p_2)}}{cef_{(p1,p2)}} is the conditional error function (of a specified family) running through the observed pair of p-values \code{(p1,p2)}.

  There are two alternative ways of specifying the family of conditional error functions (i.e., the test): through a type \code{typ}, or through an initial conditional error function \code{fun} and a distortion method \code{dis}; see \code{\link{CEF}} for details.

  If p1 and p2 are of length 1, a single overall p-value is computed (and not plotted). Otherwise, the behavior of \code{ovP} depends on \code{grid}:
  \itemize{
    \item If \code{grid = FALSE}, overall p-values are computed (and not plotted) for the elementwise pairs of \code{p1} and \code{p2}. Here, \code{p1} and \code{p2} must be of the same length. 
    \item If \code{grid = TRUE}, a grid is spanned by \code{p1} and \code{p2}, and p-values are computed (and possibly plotted) over this grid. Here, \code{p1} and \code{p2} may be of different length. Plotting is triggered by \code{plt = TRUE}, and the style of the plot (wireframe or cloud) is determined by \code{wire}. \code{invisible = TRUE} suppresses the printing of the p-values.
    }
  The p-values are rounded to \code{round} digits after the comma (\code{round = TRUE} rounds to 1 digit; \code{round = FALSE} and \code{round = 0} prevent rounding). The plot always shows unrounded values.
}
\value{
  A p-value, a vector of p-values or a matrix of p-values.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Brannath, W., Posch, M., Bauer, P. (2002). Recursive combination tests. \emph{J. Amer. Statist. Assoc.} 97, 236-244.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.
  
  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
\note{
  Provide either \code{typ} or \code{fun}, not both! If \code{fun} is provided, then also specify \code{dis}.
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{CEF}}}
\examples{
## Visualize a Lehmacher Wassmer (1999) test to the overall level 0.1
## and compute and visualize the overall p-value for an observed (p1,p2)=(0.3,0.7)
alpha  <- .1
alpha0 <- .5
alpha1 <- .05
plotBounds(a1=alpha1, a0=alpha0, add=FALSE)
plotCEF(typ="l", a2=tsT(typ="l", a=alpha, a0=alpha0, a1=alpha1))
plotCEF(typ="l", p1=.3, p2=.7)
ovP(typ="l", p1=.3, p2=.7, a1=alpha1, a0=alpha0)
# The overall p-value is the area left of alpha1, plus the area below the 
# conditional error function running though (0.3,0.7) between alpha1 and alpha0.

## Investigate the p-values of the Lehmacher Wassmer (1999) test from above
ovP(typ="l", a1=alpha1, a0=alpha0, grid=TRUE, p1=1:9/10, round=3)
ovP(typ="l", a1=alpha1, a0=alpha0, grid=TRUE, plt=TRUE, invisible=TRUE, wire=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
