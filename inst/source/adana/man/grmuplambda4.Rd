\name{grmuplambda4}
\alias{grmuplambda4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu+Lambda replacement function 4 
}
\description{
In the current population, randomly selected \eqn{\lambda} parents are replaced by randomly selected \eqn{\lambda} offspring.
}
\usage{
grmuplambda4(parpop, offpop, lambda, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parpop}{A matrix. Parent population}
  \item{offpop}{A matrix. Offspring population}
  \item{lambda}{Number of individuals renewed in the population}
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
 \code{\link{grmuvlambda}}, 
 \code{\link{grrobin}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}
