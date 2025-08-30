\name{ssrx}
\alias{ssrx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mixed replacement function
}
\description{
The offspring with the best fitness value takes the place of an individual randomly selected from among the individuals excluding their parents and the individual with the worst fitness value in the population.
}
\usage{
ssrx(parpop, offpop, reppars, ...)
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
 \code{\link{ssrfamtour}}
}
