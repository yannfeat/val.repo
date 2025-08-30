\name{selwscale}
\alias{selwscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Window Scaling
}
\description{
Window Scaling is a method based on subtracting the worst fitness value from the other fitness values. In this case, since the scaled values of the worst fit individuals will be 0, these individuals will not be given a chance to be selected.
}
\usage{
selwscale(fitvals, ns, fmin, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{fmin}{The number to subtract from all fitness values.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{select}},
  \code{\link{selrand}},
  \code{\link{selrswrp}},
  \code{\link{selrws}},
  \code{\link{selrws2}},
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selrss}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selrscale2}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}}       
}
\examples{
fitvals = c(6.1, 3.5, 6.2, 4.4, 5.2)
fmin = min(fitvals)
cnames = paste0("C",1:length(fitvals))
matpool = selwscale(fitvals, fmin=fmin)
cat(cnames[matpool],"\n")
fitvals = fitvals[matpool]
fitvals
matpool = selwscale(fitvals, fmin=fmin)
cat(cnames[matpool],"\n")
fitvals = fitvals[matpool]
fitvals
fmin = min(fitvals)
matpool = selwscale(fitvals, fmin=fmin)
cat(cnames[matpool],"\n")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
