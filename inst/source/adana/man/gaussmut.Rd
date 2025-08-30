\name{gaussmut}
\alias{gaussmut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gauss Mutation
}
\description{
Gauss Mutation is an operator made by adding randomly selected values from a normal distribution with a mean of 0 and a standard deviation of sigma to a randomly selected gene in the chromosome (Michalewicz, 1995; Back et.al., 1991; Fogel, 1995).

This operator is used for value encoded (integer or real number) chromosomes.
}
\usage{
gaussmut(y, mutsdy, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector. Chromosome of the offspring}
  \item{mutsdy}{A vector. Vector of standard deviations of genes}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{mutant}{A vector. Chromosome of the offspring}
  \item{mutgen}{The number of the mutated gene.}
}
\references{
Michalewicz, Z. (1995). Genetic algorithms, numerical optimizations and constraints. In  \emph{Proc. of the 6th. Int. Conf. on Genetic Algorithms}, pp. 151-158. Morgan Kaufmann.

Back, T., Hoffmeister, F. and Schwefel, H.F. (1991). A survey of elolution strategies. In \emph{Proc. of the 4th. Int. Conf. on Genetic Algorithms (eds. R.K. Belew and L.B. Booker)}, pp. 2-9. Morgan Kaufmann.

Fogel D.B. (1995). Evolutionary computation. Toward a new philosophy of machine intellegence. Piscataway, NJ: IEEE Press. 
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
mutsdy = c(1, 1.5, 1.01, 0.4, 1.5, 1.2)
offspring = c(8, 6, 4, 1, 3, 7)
set.seed(12)
gaussmut(offspring)
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
