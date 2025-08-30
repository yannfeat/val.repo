\name{plotCEF}
\alias{plotCEF}
\title{Function to plot a conditional error function}
\description{
  This function plots a conditional error function.
}
\usage{
plotCEF(typ = NA, fun = NA, dis = NA, a2 = NA, c = NA, p1 = NA, p2 = p1,
        x = 0:200/200, add = TRUE, xlim = c(0, 1), ylim = c(0, 1),
        plt.pt = TRUE, plt.ptann = TRUE, xlab = NA, ylab = NA, ...)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{fun}{a conditional error function}
  \item{dis}{a distortion method for a supplied conditional error function (see details): \code{"pl"} for power lines, \code{"vt"} for vertical translation}
  \item{a2}{\eqn{\alpha_2}{alpha2}, the local level of the test after the second stage}
  \item{c}{the parameter \eqn{c}}
  \item{p1}{the p-value \eqn{p_1}{p1} of the test after the first stage}
  \item{p2}{the p-value \eqn{p_2}{p2} of the test after the second stage, defaults to \code{p1}}
  \item{x}{vector on which the conditional error function is plotted (should be relatively dense in [0,1])}
  \item{add}{logical determining whether the bounds should be added to an existing plot (default) or a new plot should be opened}
  \item{xlim}{the x limits of the plot (default: \code{c(0, 1)}; other choices can be used to "zoom in")}
  \item{ylim}{the y limits of the plot (default: \code{c(0, 1)}; other choices can be used to "zoom in")}
  \item{plt.pt}{logical determining whether the point that the conditional error function is made to run through should be plotted or not (default: yes)}
  \item{plt.ptann}{logical determining whether the point that the conditional error function is made to run through should be annotated or not (default: yes)}
  \item{xlab}{a label for the x axis (default: no label)}
  \item{ylab}{a label for the y axis (default: no label)}
  \item{\dots}{arguments to be passed on to the underlying \code{plot} and \code{points} functions (e.g., graphical parameters)}
}
\details{
  There are two alternative ways of specifying the desired conditional error function:
  \itemize{
    \item through a type \code{typ}, and either a parameter (either \code{a2} or \code{c}) or a point \code{(p1,p2)}, OR
    \item through an initial conditional error function \code{fun}, and possibly a distortion method \code{dis} together with either the parameter \code{a2} or a point \code{(p1,p2)}
     }

  Most people will only need the first of these two ways; the second leads to user-defined non-standard tests.

  If \code{typ} is specified, a parameter \code{a2} or \code{c} or the point \code{(p1,p2)} must be provided. In this case, \code{plotCEF} plots the conditional error function of the chosen type with the given parameter or running through the given point.

  If \code{typ} is not specified, a conditional error function (i.e., a nonincreasing function defined on [0,1] with values in [0,1]) \code{fun} must be provided. If no distortion method is selected (\code{dis = NA}), \code{fun} is plotted unchanged. Otherwise, the function is distorted using the chosen distortion method, either to match a desired second stage level \code{a2} or to run through a specified point \code{(p1,p2)} (one of which must be provided). Currently, two distortion methods are implemented:
  \itemize{
    \item \code{dis = "pl"}, Power lines: For an initial function \code{fun}, define \eqn{f[r](x) = (f(x^r))^(1/r)}, r>0. Note that if \code{fun} is a conditional error function of type \code{"b"} (Bauer and Koehne, 1994), so is f[r].
    \item \code{dis = "pl"}, Vertical translation: The initial function \code{fun} is shifted vertically.
    }

  See \code{\link{parconv}} for more information on the two alternative parameterizations by \eqn{\alpha_2}{alpha2} and \eqn{c}.

  Internally, \code{plotCEF} uses \code{\link{CEF}} to compute the conditional error function that is to be plotted.
}
\value{
  The function \code{plotCEF} is invoked for its plotting effect; it returns no meaningful value.
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

  \code{plt.pt} and \code{plt.ptann} are not considered if \code{p1 = NA}. \code{plt.ptann} is not considered if \code{plt.pt = FALSE}.

  Note that in this implementation of adaptive two-stage tests, early stopping bounds are \emph{not} part of the conditional error function. Rather, they are specified separately (see also \code{\link{tsT}}).
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{parconv}}, \code{\link{CEF}}, \code{\link{tsT}}}
\examples{
## Plot two conditional error functions of the Lehmacher-Wassmer (1999) type:
## one to the local level alpha2=0.1, and one that runs through (p1,p2)=(0.3,0.7)
plotCEF(typ="l", a2=0.1, add=FALSE)
plotCEF(typ="l", p1=0.3, p2=0.7)

## Plot an explicitly defined conditional error function, and distort it
plotCEF(fun=function(x) ifelse(x<.5,(1-x)^2, (1-x)/2), add=FALSE)
plotCEF(fun=function(x) ifelse(x<.5,(1-x)^2, (1-x)/2), dis="pl", a2=.5)
foo <- CEF(fun=function(x) ifelse(x<.5,(1-x)^2, (1-x)/2), dis="pl", a2=.5)
plotCEF(fun=foo, col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
