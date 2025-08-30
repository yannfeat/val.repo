\name{insswapmut}
\alias{insswapmut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insertion + Inversion Mutation
}
\description{
It is a mutation operator that combines insertion and inversion mutation.

This operator is used in problems with permutation encoding.
}
\usage{
insswapmut(y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{\dots}{Further arguments passed to or from other methods.}
}

\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{A vector. The numbers of begining and ending of the mutated genes.}
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
  \code{\link{invdismut}}
}
\examples{
offspring = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
insswapmut(offspring)
}
