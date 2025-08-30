\name{ssrmup1}
\alias{ssrmup1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu+1 replacement function
}
\description{
Two randomly selected parents from the mating pool are mated to produce one or more offspring. The fit value of an individual randomly selected from the population is compared with the offspring with the highest fitness value. If the fitness value of the offspring is higher, the offspring is replaced with the individual.
}
\usage{
ssrmup1(parpop, offpop, ...)
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
 \code{\link{grmuvlambda}}, 
 \code{\link{grrobin}},
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}
