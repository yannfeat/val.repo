\name{grmuplambda}
\alias{grmuplambda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu+Lambda replacement function 1
}
\description{
The Mu+Lamda replacement is based on the selection of the fittest parents and offspring as individuals of the new generation population (Smith et.al., 1999; Jenkins et.al., 2019).
}
\usage{
grmuplambda(parpop, offpop, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parpop}{A matrix. Parent population}
  \item{offpop}{A matrix. Offspring population}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
   A matrix. Population of the new generation.
}
\references{
Smith, A.E. and Vavak F. (1999). Replacement strategies in steady state genetic algorithms: Static environments. \emph{Foundations of Genetic Algorithms}. pp. 499-505.

Jenkins, A., Gupta, V., Myrick, A. and Lenoir, M. (2019). Variations of Genetic Algorithms. \emph{arXiv preprint arXiv:1911.00490}.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{grdelall}},
 \code{\link{elitism}}, 
 \code{\link{grmuplambda2}}, 
 \code{\link{grmuplambda3}}, 
 \code{\link{grmuplambda4}}, 
 \code{\link{grmuvlambda}}, 
 \code{\link{grrobin}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}
