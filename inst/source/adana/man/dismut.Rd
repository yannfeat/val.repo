\name{dismut}
\alias{dismut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displacement mutation
}
\description{
The Displacement mutation cuts the genes between two randomly determined cut-points from the chromosome as a subset and then inserts them, starting from a randomly selected location (Michalewicz, 1992). 

This operator is used in problems with permutation encoding.
}
\usage{
dismut(y, ...)
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
\references{
Michalewicz, Z. (1992). \emph{Genetic Algorithms + Data Structures = Evolution Programs}. Berlin-Heidelberg: Springer Verlag.
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
  \code{\link{bsearchmut2}},   
  \code{\link{swapmut}},   
  \code{\link{invmut}},   
  \code{\link{shufmut}},   
  \code{\link{insmut}},  
  \code{\link{invswapmut}},
  \code{\link{insswapmut}},
  \code{\link{invdismut}}
}
\examples{
offspring = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
dismut(offspring)
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
