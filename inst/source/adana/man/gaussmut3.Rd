\name{gaussmut3}
\alias{gaussmut3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gauss Mutation 3
}
\description{
GM is an operator made by adding randomly selected values from a normal distribution with mean and standard deviation of MU and SIGMA, respectively, to a randomly selected gene in the chromosome.

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
gaussmut3(y, mutmy, mutsdy, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{mutmy}{A vector. Vector of means of genes}
  \item{mutsdy}{A vector. Vector of standard deviations of genes}
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
  \code{\link{powmut2}},   
  \code{\link{gaussmut}},   
  \code{\link{gaussmut2}},   
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
mutmy = c(5, 5, 2, 4, 3, 4)
mutsdy = c(1, 1.5, 1.01, 0.4, 1.5, 1.2)
offspring = c(8, 6, 4, 1, 3, 7)
set.seed(12)
gaussmut(offspring, mutmy=mutmy, mutsdy=mutsdy)
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
