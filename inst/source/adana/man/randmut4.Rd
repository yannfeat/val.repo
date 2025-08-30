\name{randmut4}
\alias{randmut4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random mutation 4 
}
\description{
An alternative random mutation operator proposed by Wijayaningrum et.al. (2017).

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
randmut4(y, lb, ub,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{lb}{A vector. Lower bounds of genes}
  \item{ub}{A vector. Upper bounds of genes}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{The number of the mutated gene.}
}
\references{
Wijayaningrum, V.N., Starkweather, T. and D'ann, F. (2017). Scheduling problemsand traveling salesman: The genetic edge recombination operators. In \emph{Proc. of ICGA}, Vol. 89, pp. 133-40.
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
lb = c(2, 1, 3, 1, 0, 4)
ub = c(10, 15, 8, 5, 6, 9)
offspring = c(8, 6, 4, 1, 3, 7)
randmut4(offspring, lb=lb, ub=ub)
}

