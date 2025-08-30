\name{seldet}
\alias{seldet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deterministic Selection
}
\description{
Deterministic Selection is similar to Remainder Stochastic Selection. The expected value of each individual in the mating pool is calculated. Individuals are copied directly into the mating pool by the exact number of expected values. Then, sorting is done according to the fraction part of the expected values. In this case, the individuals with the highest fractions go to the top of the list to be selected. The number of individuals required to complete the mating pool to population size is selected by going from the beginning of the list to the end.
}
\usage{
seldet(fitvals, ns, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
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
matpool = seldet(fitvals)
cat("Selected chromosomes: ", cnames[matpool], "\n")
}

