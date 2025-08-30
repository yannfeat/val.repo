\name{ssrgenitor}
\alias{ssrgenitor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Genitor replacement function
}
\description{
The offspring obtained by mating two randomly selected parents from the mating pool is placed in the place of the worst individual in the current population (Whitley, 1988).
}
\usage{
ssrgenitor(parpop, offpop, ...)
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
Whitley, L.D. (1988). GENITOR: A different genetic algorithm. In \emph{Proc. of the Rocky Mountain conf. on artificial intellegence}, pp. 118-130.
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
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}
}
