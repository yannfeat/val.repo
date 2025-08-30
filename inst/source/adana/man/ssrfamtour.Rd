\name{ssrfamtour}
\alias{ssrfamtour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replacement function via family tournament
}
\description{
The two most compatible between the two parents and their offspring are added to the new generation population, while those with low fitness are discarded (Sivanandam et.al., 2007).
}
\usage{
ssrfamtour(parpop, offpop, reppars, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parpop}{A matrix. Parent population}
  \item{offpop}{A matrix. Offspring population}
  \item{reppars}{A vector. Indices of the parents}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
   A matrix. Population of the new generation.
}
\references{
Sivanandam et.al., (2007).
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
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrx}}
}

