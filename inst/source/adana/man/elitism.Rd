\name{elitism}
\alias{elitism}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Elistist Replacement (Elitism) Function
}
\description{
The reproduction of individuals with the highest fitness is called elitism. The elitism operator copies a certain number of individuals into the new population. Other individuals are selected from among the offspring in proportion to their fitness values.
}
\usage{
elitism(parpop, offpop, reps, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parpop}{A matrix. Parent population}
  \item{offpop}{A matrix. Offspring population}
  \item{reps}{Number of elite individuals}
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
 \code{\link{grmuplambda}}, 
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
