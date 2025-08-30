\name{grrobin}
\alias{grrobin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Round Robin Replacement Function
}
\description{
The parent and offspring populations are combined. Then, each individual in the combined population is compared with k randomly selected individuals. In these double tournaments, if an individual has higher fitness than the individual they are compared to, +1 point is obtained. The new population is created from the individuals with the highest score.
}
\usage{
grrobin(parpop, offpop, repk, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parpop}{A matrix. Parent population}
  \item{offpop}{A matrix. Offspring population}
  \item{repk}{Number of Comparisons}
  \item{\dots}{Further arguments passed to or from other methods.}
  }
\value{
   A matrix. Population of the new generation.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{grdelall}},
 \code{\link{elitism}}, 
 \code{\link{grmuplambda}}, 
 \code{\link{grmuplambda2}}, 
 \code{\link{grmuplambda3}}, 
 \code{\link{grmuplambda4}}, 
 \code{\link{grmuvlambda}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}

