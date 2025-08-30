\name{nunimut}
\alias{nunimut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-uniform Mutation
}
\description{
The nunimut operator is a mutation operator that adjusts for generations by reducing the mutation severity according to genetic progression (Michalewicz, 1994).

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
nunimut(y, lb, ub, g, gmax, mutb, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{lb}{A vector. Lower bounds of genes}
  \item{ub}{A vector. Upper bounds of genes}
  \item{g}{Current generation number.}
  \item{gmax}{Maximum generation number.}
  \item{mutb}{An exponent parameter that sets non-uniformity}  
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{The number of the mutated gene.}
}
\references{
Michalewicz, . (1994).
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{mutate}},
  \code{\link{bitmut}},
  \code{\link{randmut}},
  \code{\link{randmut2}},
  \code{\link{randmut3}},
  \code{\link{randmut4}},
  \code{\link{unimut}},
  \code{\link{boundmut}},
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
lb = c(2, 1, 3, 1, 0, 4)
ub = c(10, 15, 8, 5, 6, 9)
offspring = c(8, 6, 4, 1, 3, 7)
set.seed(12)
nunimut(offspring, lb=lb, ub=ub, g=1, gmax=100, mutb=0.5)
set.seed(12)
nunimut(offspring, lb=lb, ub=ub, g=50, gmax=100, mutb=0.5)
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
