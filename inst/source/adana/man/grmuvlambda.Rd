\name{grmuvlambda}
\alias{grmuvlambda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu & Lambda Replacement Function
}
\description{
In this renewal strategy, after the offspring are ranked according to their fitness values, the number of the population of the offspring with the best fitness value is replaced by the parents (Schwefel, 1981). To use this renewal algorithm, it is necessary to produce many more offspring than the population count.
}
\usage{
grmuvlambda(parpop, offpop, ...)
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
\author{
Zeynel Cebeci & Erkut Tekeli
}


\seealso{
 \code{\link{grdelall}},
 \code{\link{elitism}}, 
 \code{\link{grmuplambda}}, 
 \code{\link{grmuplambda2}}, 
 \code{\link{grmuplambda3}}, 
 \code{\link{grmuplambda4}}, 
 \code{\link{grrobin}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}

