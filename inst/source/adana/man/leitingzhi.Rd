\name{leitingzhi}
\alias{leitingzhi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lei & Tingzhi Adaptation Function
}
\description{
This adaptation function proposed by Lei & Tingzhi (1994) is an adaptation function that takes into account the cooperation of individuals.
}
\usage{
leitingzhi(fitvals, cxpc, cxpc2, 
                      mutpm, mutpm2, adapa, adapb, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{A vector. Fitness values of current generation}
  \item{cxpc}{Crossover rate for adaptation. 0 <= cxpc <= 1. default is 0.9}
  \item{cxpc2}{Crossover rate for adaptation. 0 <= cxpc2 <= 1. default is 0.5}
  \item{mutpm}{Mutation  rate for adaptation. 0 <= mutpm <= 1. default is 0.05}
  \item{mutpm2}{Mutation  rate for adaptation. 0 <= mutpm2 <= 1. default is 0.2}
  \item{adapa}{Adaptation threshold for average of fitness values. 0 <= adapa <= 1. default is 0.7}
  \item{adapb}{Adaptation threshold for minimum of fitness values. 0.5 <= adapb <= 1. default is 0.5}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{pc}{Crossover rate}
  \item{pm}{Mutation rate}
}
\references{
Lei, W. and Tingzhi, S. (2004). An improved adaptive genetic algorithm and its application to image segmentation. In \emph{Proc. of 5th Int. Conf. on Artificial Neural Network and Genetic Algorithms}, pp. 112-119.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{fixpcmut}},
 \code{\link{ilmdhc}},
 \code{\link{adana1}},
 \code{\link{adana2}},
 \code{\link{adana3}}
}

