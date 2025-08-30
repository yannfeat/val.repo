\name{adana3}
\alias{adana3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dynamic mutation and crossover function (Adana 3)
}
\description{
This adaptation function proposed by Cebeci (2021) is an adaptation function that takes into account the cooperation of individuals.
}
\usage{
adana3(fitvals, g, gmax, cxpc, mutpm,
                  adapc, adapd, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{A vector. Fitness values of current generation}
  \item{g}{Current generation}
  \item{gmax}{Maximum generation}
  \item{cxpc}{Crossover rate. 0 <= cxpc <= 1}
  \item{mutpm}{Mutation  rate. 0 <= mutpm <= 1}
  \item{adapc}{Adaptation threshold for crossover rate. 0 <= adapa <= 1. default is 0.05}
  \item{adapd}{Adaptation threshold for mutation rate. 0 <= adapb <= 1. default is 0.05}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{fixpcmut}},
 \code{\link{ilmdhc}},
 \code{\link{adana1}},
 \code{\link{adana2}},
 \code{\link{leitingzhi}}
}
