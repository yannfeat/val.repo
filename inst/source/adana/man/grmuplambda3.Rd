\name{grmuplambda3}
\alias{grmuplambda3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu+Lambda replacement function 3 
}
\description{
After the offspring are ranked according to their fitness values, the \eqn{\lambda} best fit offspring are replaced by \eqn{\lambda} parents randomly selected from the current parent population.
}
\usage{
grmuplambda3(parpop, offpop, lambda, ...)
}
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
 \code{\link{grmuplambda4}}, 
 \code{\link{grmuvlambda}}, 
 \code{\link{grrobin}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}}
