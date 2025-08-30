\name{pbx2}
\alias{pbx2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Position-Based Crossover 2 (PBX2)
}
\description{
The Position-Based Crossover (PBX) operator inserts a different number of randomly selected genes in one parent into the same position in Offspring1, then rounds off the other genes in sequence according to their positions in the other parent (Syswerda, 1991). Other offspring are generated similarly if desired or necessary. PBX is an operator that tries to ensure diversity in recombination while taking care to preserve position.
}
\usage{
pbx2(x1, x2, cxon, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{A vector. It contains the chromosomal information of parent-1.}
  \item{x2}{A vector. It contains the chromosomal information of parent-2.}
  \item{cxon}{Number of offspring to be generated as a result of crossover}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A matrix containing the generated offsprings.
}
\references{
Syswerda, G. (1991). Schedule optimization using genetic algorithms. \emph{Handbook of Genetic Algorithms}.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

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
  \code{\link{ebx}},
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
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}
}
\examples{
ebeveyn1 = c(3, 4, 8, 2, 7, 1, 6, 5)
ebeveyn2 = c(4, 2, 5, 1, 6, 8, 3, 7)
pbx2(ebeveyn1, ebeveyn2, cxon=2)
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
