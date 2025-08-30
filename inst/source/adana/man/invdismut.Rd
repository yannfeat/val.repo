\name{invdismut}
\alias{invdismut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displacement + Inversion Mutation
}
\description{
It is a mutation operator that combines displacement and inversion mutation.

This operator is used in problems with permutation encoding.
}
\usage{
invdismut(y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutrange}{A vector. The numbers of begining and ending of the mutated genes.}
  \item{r}{The number of insertation location.}
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
  \code{\link{insswapmut}}
}
\examples{
offspring = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
invdismut(offspring)
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
