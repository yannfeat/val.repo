\name{adana1}
\alias{adana1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adaptive Dynamic Algorithm (Adana 1)
}
\description{
Adana-1 is an adaptation function that calculates the mutation rates to be applied in generations by sine wave modeling (Cebeci, 2021).
}
\usage{
adana1(g, gmax, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{Current generation}
  \item{gmax}{Maximum generation}
  \item{\dots}{Further arguments passed to or from other methods.}
}

\value{
  \item{pc}{Crossover rate}
  \item{pm}{Mutation rate}
}
\references{
Cebeci, Z. (2021). R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari, 535 p. Ankara:Nobel Akademik Yayincilik. 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{fixpcmut}},
 \code{\link{ilmdhc}},
 \code{\link{adana2}},
 \code{\link{leitingzhi}},
 \code{\link{adana3}}
}
\examples{
gmax <- 1000
g <- c(1, 10, 50, 100, 250, 500, 750, gmax)
adana1(g=g, gmax=gmax)
}

