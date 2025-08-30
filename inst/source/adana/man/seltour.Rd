\name{seltour}
\alias{seltour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tournament Selection
}
\description{
The best one is selected in the group consisting of t individuals selected by random sampling with or without replacement from the current population (Smith et.al, 1991).
}
\usage{
seltour(fitvals, ns, selt, reptype, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{selt}{Number of tournament size}
  \item{reptype}{Type of Sampling, \code{TRUE : without repetitions}, \code{FALSE : with repetitions}} 
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\references{
Smith, R.E., Goldberg, D.E. and Earickson, J.A. (1991). SGA-C: A C-language implementation of a simple gewnetic algorithm. Technical report 91002, Illinois Genetic Algorithms Laboratory, Urbana, IL, USA.
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
  \code{\link{seltour2}}    
}

\examples{
selt = 2                               # Size of tournament
fitvals = c(6, -1, 2, 4, 5)            # Fitness values
cnames = paste0("C",1:length(fitvals)) # Chromosome names
matpool = seltour(fitvals, selt=selt)
cat(cnames[matpool],"\n")
}
