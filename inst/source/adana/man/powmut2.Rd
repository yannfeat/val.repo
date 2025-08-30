\name{powmut2}
\alias{powmut2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power Mutation 2
}
\description{
Power Mutation is an operator that generates a mutation in a random gene at a certain power of a random number. In this operator, a different exponent parameter can be given for each gene.

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
powmut2(y, lb, ub, mutpow, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{lb}{A vector. Lower bounds of genes}
  \item{ub}{A vector. Upper bounds of genes}
  \item{mutpow}{A vector of exponent parameter} 
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{The number of the mutated gene.}
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
  \code{\link{nunimut}},  
  \code{\link{nunimut2}},
  \code{\link{powmut}},   
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
mutpow = c(3, 0.5, 0.5, 2, 3, 1)
offspring = c(8, 6, 4, 1, 3, 7)
set.seed(12)
powmut2(offspring, lb=lb, ub=ub, mutpow=mutpow)
}
