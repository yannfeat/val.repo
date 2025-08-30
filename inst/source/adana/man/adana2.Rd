\name{adana2}
\alias{adana2}

\title{
Adaptive Dynamic Algorithm (Adana 2)
}
\description{
Adana-2 is an adaptation function that calculates the mutation rates to be applied in generations by square root modeling (Cebeci, 2021).
}
\usage{
adana2(g, gmax, ...)
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{fixpcmut}},
 \code{\link{ilmdhc}},
 \code{\link{adana1}},
 \code{\link{leitingzhi}},
 \code{\link{adana3}}
}
\examples{
gmax <- 1000
g <- c(1, 10, 50, 100, 250, 500, 750, gmax)
adana1(g=g, gmax=gmax)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
