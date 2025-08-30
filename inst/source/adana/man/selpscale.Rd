\name{selpscale}
\alias{selpscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power-law Scaling
}
\description{
the Power-law Scaling is a selection method in which the \emph{k}th power of the fit values is used as the scaled fit values (Gillies, 1985).
}
\usage{
selpscale(fitvals, ns, selk, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{selk}{Power factor} 
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\references{
Gillies, A.M. (1985). Machine learning procedures for generating image domain feature detectors. PhD thesis, University of Michigan, Ann Arbor.  
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
  \code{\link{selrss}},  
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selwscale}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},  
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selrscale2}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}}    
}
\examples{
fitvals = c(6.1, 3.5, 6.2, 4.4, 5.2)
cnames = paste0("C",1:length(fitvals))
matpool = selpscale(fitvals, selk=1.1)
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
