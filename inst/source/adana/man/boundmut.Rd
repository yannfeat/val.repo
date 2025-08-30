\name{boundmut}
\alias{boundmut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Boundary Mutation
}
\description{
The Boundary Mutation operator is a mutation operator that changes the value of a randomly selected gene in the chromosome with the upper or lower limit value for that gene.

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
boundmut(y, lb, ub, ...)
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
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mutate}},
  \code{\link{bitmut}},
  \code{\link{randmut}},
  \code{\link{randmut2}},
  \code{\link{randmut3}},
  \code{\link{randmut4}},
  \code{\link{unimut}},
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
boundmut(offspring, lb=lb, ub=ub)
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
