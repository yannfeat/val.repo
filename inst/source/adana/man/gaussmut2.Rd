\name{gaussmut2}
\alias{gaussmut2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gauss Mutation 2
}
\description{
Gauss Mutation-2 is an operator by adding a randomly selected value from the standard normal distribution to a randomly selected gene in the chromosome.

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
gaussmut2(y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
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
  \code{\link{boundmut}},
  \code{\link{nunimut}},  
  \code{\link{nunimut2}},
  \code{\link{powmut}},   
  \code{\link{powmut2}},   
  \code{\link{gaussmut}},   
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
offspring = c(8, 6, 4, 1, 3, 7)
set.seed(12)
gaussmut2(offspring)
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
