\name{grmuplambda2}
\alias{grmuplambda2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mu+Lambda replacement function 2 (delete the worst \eqn{\lambda})
}
\description{
Parents and offspring are ranked separately according to their compatibility among themselves. Then \eqn{\lambda} offspring with the best fitness value is replaced by \eqn{\lambda} parent with the worst fitness value.
}
\usage{
grmuplambda2(parpop, offpop, lambda, ...)
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

