\name{findoptima}
\alias{findoptima}
\title{Finds peaks and valleys on the curve of a function with single variable}
\description{This function finds the peaks and valleys on the curve of user-defined functions with one variable. 
  The function also plots the function curve that can be used to demonstrate the points for local optima and 
  global optimum in a optimization problem. }
\usage{
findoptima(x, type="max", pflag=TRUE)
}
\arguments{
  \item{x}{a vector of variable}
  \item{type}{Either \dQuote{max} (the default) or \dQuote{min}. The peaks are 
              found when \code{type="max"} and the valleys are found
              when \code{type="max"}.}
  \item{pflag}{if this is TRUE, the first and last values are also checked.}
}
\details{
The findoptima function finds all the peaks and valleys in a given function curve. The points can be colorized with different colors. See the example below.
}
\value{
Returns a vector of indices of the peaks or valleys on the function curve.
}
\references{
Cebeci Z. (2021). \emph{R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari}. Ankara:Nobel Akademik Yayincilik}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\examples{
fx <- function(x) -sin(x)-sin(2*x)-cos(3*x) + 3
x <- seq(-2*pi, 2*pi, by=0.001)
curve(fx, x)
cr <- curve(fx, x, lwd=2)
xy <- cbind(cr$x, cr$y)
peaks <- findoptima(cr$y, type = "max")
valleys <- findoptima(cr$y, type = "min")

## Finds peaks and valleys
peaks <- findoptima(cr$y, type="max")
valleys <-  findoptima(cr$y, type="min")

## Plotting the function curve and local optima and global optimum
points(xy[peaks,], pch=19, cex=1.2, col=2)
points(xy[valleys,], pch=18, cex=1.2, col=4)
gmin <- valleys[which.min(xy[valleys,2])]
gmax <- peaks[which.min(xy[valleys,2])]
points(xy[gmax,1], xy[gmax,2], pch=19, cex=2, col=2)
points(xy[gmin,1], xy[gmin,2], pch=18, cex=2, col=4)
text(xy[gmax,1], xy[gmax,2], labels="Glob.Max", pos=2, cex=0.8, col=1)
text(xy[gmin,1], xy[gmin,2], labels="Glob.Min", pos=2, cex=0.8, col=1)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ ~optimize}
\keyword{ ~graphs}

% For non-standard keywords, use \concept instead of \keyword:
\concept{ ~local optima}
\concept{ ~global optimum}
\concept{ ~optimization}
\concept{ ~genetic algorithms}
\concept{ ~evolutionary computing}

