\name{selrss}
\alias{selrss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remainder Stochastic Selection
}
\description{
The fitness probability of individuals is multiplied by the population size to calculate the number of times the individual will reproduce in the mating pool, ie the expected number of copies. The expected number of copies is a fractional number. An exact fraction of the expected number of copies of the individual is sent to the mating pool. It is also determined whether it can go back to the mating pool for the fraction part (Brindle, 1981).
}
\usage{
selrss(fitvals, ns, ...)
}
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\references{
Brindle, A. (1981). Genetic algorithms for function optimization. PhD thesis, University of Alberta.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{select}},
  \code{\link{selrand}},
  \code{\link{selrswrp}},
  \code{\link{selrws}},
  \code{\link{selrws2}} ,
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
  \code{\link{seltour2}}       
}
\examples{
fitvals = c(6, 1, 2, 4, 5)
cnames = paste0("C",1:length(fitvals))
matpool = selrss(fitvals)
cat("Selected Chromosomes: ", cnames[matpool], "\n")
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
