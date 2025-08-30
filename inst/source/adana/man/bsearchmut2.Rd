\name{bsearchmut2}
\alias{bsearchmut2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Boundary Search Mutation 2
}
\description{
Boundary Search Mutation-2 is an algorithm based on searching the convenience region boundaries in constraint processing for NLP optimization (Michalewicz & Schoenauer, 1996). Two genes are randomly selected from the chromosome and one is multiplied by the random value p, while the other gene is multiplied by the q value calculated using p.

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
bsearchmut2(y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{A vector. The numbers of the mutated genes.}
}
\references{
Michalewicz, Z. and Schoenauer, M. (1996). Evolutionary algorithms for constrained parameter optimization problems. \emph{Evolutionary Computation}, 4(1), 1-32. 
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
  \code{\link{gaussmut2}},   
  \code{\link{gaussmut3}},   
  \code{\link{bsearchmut1}},        
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
offspring = c(8, 6, 4, 1, 3)
bsearchmut2(offspring)
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
