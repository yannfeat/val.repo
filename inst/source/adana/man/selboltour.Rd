\name{selboltour}
\alias{selboltour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Boltzmann Tournament Selection
}
\description{
In the Boltzman tournament, the initial selection pressure is low. Therefore, every individual, whether low or high fitness value, has a chance to be selected. In the following generations, the selection pressure gradually increases. In other words, individuals with high fitness value are forced to be selected.
}
\usage{
selboltour(fitvals, ns, selt0, selg, selgmax, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{selt0}{Number, Initial temperature}
  \item{selg}{Current generation number}
  \item{selgmax}{Maximum generation number}
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
  \code{\link{seltour2}}  
}
\examples{
fitvals = c(6, -1, 2, 4, 5)             # Fitness Values
cnames = paste0("C",1:length(fitvals))  # Chromosome Names
matpool = selboltour(fitvals, selt0=100, selg=5, selgmax=100)
cat(cnames[matpool],"\n")
matpool = selboltour(fitvals, selt0=100, selg=95, selgmax=100)
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
