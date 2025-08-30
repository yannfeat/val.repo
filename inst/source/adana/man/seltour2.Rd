\name{seltour2}
\alias{seltour2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tournament Selection 2
}
\description{
Each individual is given a chance to participate in the tournament at least once in selection by tournament in this function. For this reason, individuals participating in the tournament cannot participate in another tournament, but after the tournament of all individuals is completed, they can get a chance to participate in another tournament (Nicolau, 2009).
}
\usage{
seltour2(fitvals, ns, selt, reptype, ...)
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
Nicolau, M. (2009). Application of a simple binary genetic algorithm to a noiseless testbed benchmark. In \emph{Proc. genetic and Evolutionary Computation Conf. (GECCO)}, Montreal, Canada.
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
  \code{\link{selboltour}},
  \code{\link{sellrs}},
  \code{\link{sellrs2}},
  \code{\link{sellrs3}},
  \code{\link{selnlrs}},
  \code{\link{selers}},
  \code{\link{seltrunc}},
  \code{\link{select}}  
}
\examples{
selt = 2                    # Size of tournament
fitvals = c(6, -1, 2, 4, 5) # Fitness values
cnames = paste0("C",1:length(fitvals)) # Chromosome names
matpool = seltour2(fitvals, selt=selt)
cat(cnames[matpool],"\n")
}
