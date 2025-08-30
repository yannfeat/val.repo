\name{seltrunc}
\alias{seltrunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Truncation Selection
}
\description{
Individuals in the population are ranked according to their fitness value and individuals with higher fitness value than a determined threshold value are included in the mating pool.
}
\usage{
seltrunc(fitvals, ns, selps, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{selps}{Percentage of Selection,  (\code{0.0 <= selps <= 1.0})}
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
  \code{\link{selrss}},  
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selwscale}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},  
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selrscale2}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}},
  \code{\link{selboltour}},
  \code{\link{sellrs}},
  \code{\link{sellrs2}},
  \code{\link{sellrs3}},
  \code{\link{selnlrs}},
  \code{\link{selers}}  
}
\examples{
fitvals = c(6, -1, 2, 4, 5) 
cnames = paste0("C",1:length(fitvals)) 
matpool = seltrunc(fitvals, selps=0.60)
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
