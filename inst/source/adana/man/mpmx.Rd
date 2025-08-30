\name{mpmx}
\alias{mpmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Partially Mapped Crossover
}
\description{
Modified Partially Mapped Crossover (MPMX) is a crossover operator for permutation encoded chromosomes. Each of the offspring uses sequencing information partially determined by each of their parents. Two different cut points are randomly determined. The part outside of the two cut points is replaced. Pieces between the two cut points are complemented from the original parental genes. However, if the same genes are found among the copied genes, they are changed.
}
\usage{
mpmx(x1, x2, cxon, ...)
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
parent1 = c(0, 8, 4, 5, 6, 7, 1, 2, 3, 9)
parent2 = c(6, 7, 1, 2, 4, 8, 3, 5, 9, 0)
mpmx(parent1, parent2)
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
