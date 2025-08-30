\name{ebx}
\alias{ebx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extended Box Crossover
}
\description{
Extended Box Crossover (EBX) was proposed by Yoon and Kim (2012) as the more advanced form of Box Crossover (BX). In the EBX operator, the minimum and maximum values are weighted by an alpha factor.
}
\usage{
ebx(x1, x2, lb, ub, cxon, cxalfa, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{A vector. It contains the chromosomal information of parent-1.}
  \item{x2}{A vector. It contains the chromosomal information of parent-2.}
  \item{lb}{A vector. Lower bounds of each gene in the chromosomes.}
  \item{ub}{A vector. Upper bounds of each gene in the chromosomes.}
  \item{cxon}{Number of offspring to be generated as a result of crossover}
  \item{cxalfa}{A vector. Alpha value for each gene in the chromosomes. If no value is entered, they are randomly selected by the function in the range [0,1].}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A matrix containing the generated offsprings.
}
\references{
Yoon, Y. and Kim, Y.H. (2012). The roles of crossover and mutation in real-coded genetic algorithms. In \emph{Bioinspired Computational Algorithms anf Their Applications (ed. S. Gao)}, London: INTECH Open Acces Publisher. pp. 65-82.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cross}},
  \code{\link{px1}},
  \code{\link{kpx}},
  \code{\link{sc}},
  \code{\link{rsc}},
  \code{\link{hux}},
  \code{\link{ux}},
  \code{\link{ux2}},
  \code{\link{mx}},
  \code{\link{rrc}},
  \code{\link{disc}},
  \code{\link{atc}},
  \code{\link{cpc}},
  \code{\link{eclc}},
  \code{\link{raoc}},
  \code{\link{dc}},
  \code{\link{ax}},
  \code{\link{hc}},
  \code{\link{sax}},
  \code{\link{wax}},
  \code{\link{lax}},
  \code{\link{bx}},
  \code{\link{blxa}},
  \code{\link{blxab}},
  \code{\link{lapx}},
  \code{\link{elx}},
  \code{\link{geomx}},
  \code{\link{spherex}},
  \code{\link{pmx}},
  \code{\link{mpmx}},
  \code{\link{upmx}},
  \code{\link{ox}},
  \code{\link{ox2}},
  \code{\link{mpx}},
  \code{\link{erx}},
  \code{\link{pbx}},
  \code{\link{pbx2}},
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}
}
\examples{
lb = c(0, 0, 0, 0, 0, 0)
ub = c(2, 3, 1, 2, 4, 3)
parent1 = c(1.1, 1.6, 0.0, 1.1, 1.4, 1.2)
parent2 = c(1.2, 0.0, 0.0, 1.5, 1.2, 1.4)
ebx(parent1, parent2, lb, ub)
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
