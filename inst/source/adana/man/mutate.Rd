\name{mutate}
\alias{mutate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function of Mutation Application
}
\description{
With mutation, the chromosomes of individuals are randomly changed and sent to the next generation.
}
\usage{
mutate(mutfunc, population, mutpm, gatype, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mutfunc}{The name of the mutation operator}
  \item{population}{A matrix. Population of offspring to be mutated}
  \item{mutpm}{Mutation Rate}
  \item{gatype}{Indicates the GA type. "gga" is assigned for generational refresh, and "ssga" for steady-state refresh.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
   A matrix. Population of the mutated offsprings
}
\references{
Cebeci, Z. (2021). R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari, 535 p. Ankara:Nobel Akademik Yayincilik. 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{bitmut}},
  \code{\link{randmut}},
  \code{\link{randmut2}},
  \code{\link{randmut3}},
  \code{\link{randmut4}},
  \code{\link{unimut}},
  \code{\link{boundmut}},
  \code{\link{nunimut}},  
  \code{\link{nunimut2}},
  \code{\link{powmut}},   
  \code{\link{powmut2}},   
  \code{\link{gaussmut}},   
  \code{\link{gaussmut2}},   
  \code{\link{gaussmut3}},   
  \code{\link{bsearchmut1}},        
  \code{\link{bsearchmut2}},   
  \code{\link{swapmut}},   
  \code{\link{invmut}},   
  \code{\link{shufmut}},   
  \code{\link{insmut}},  
  \code{\link{dismut}},
  \code{\link{invswapmut}},
  \code{\link{insswapmut}},
  \code{\link{invdismut}}
}
\examples{
offsprings=initbin(25,5)
offsprings[,"fitval"] = evaluate(maxone, offsprings[,1:(ncol(offsprings)-2)])
head(offsprings, 4)          # mutant individual may be further ahead.
mutatedpop = mutate(mutfunc=bitmut, population=offsprings, mutpm=0.1, gatype="gga")
mutatedpop[,"fitval"] = evaluate(maxone, mutatedpop[,1:(ncol(mutatedpop)-2)])
head(mutatedpop, 4)
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
