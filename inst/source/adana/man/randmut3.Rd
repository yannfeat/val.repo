\name{randmut3}
\alias{randmut3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random mutation 3
}
\description{
For each gene, if a random number is less than the mutation rate, the gene's mean value is zero and its standard deviation is |ub-lb| The random value selected from the normal distribution is changed by adding it (Yoon & Kim, 2012).

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
randmut3(y, lb, ub, mutpm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{lb}{A vector. Lower bounds of genes}
  \item{ub}{A vector. Upper bounds of genes}
  \item{mutpm}{Mutation rate}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{The number of the mutated gene.}
}
\references{
Yoon, Y. and Kim, Y.H. (2012). The roles of crossover and mutation in real-coded  genetic algorithms. In \emph{Bioinspired Computational Algorithms and Their Applications (ed. S. Gao)}. London: INTECH Open Access Publisher. pp. 65-82.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{mutate}},
  \code{\link{bitmut}},
  \code{\link{randmut}},
  \code{\link{randmut2}},
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
lb = c(2, 1, 3, 1, 0, 4)
ub = c(10, 15, 8, 5, 6, 9)
offspring = c(8, 6, 4, 1, 3, 7)
randmut3(offspring, lb=lb, ub=ub, mutpm=0.1)
}

