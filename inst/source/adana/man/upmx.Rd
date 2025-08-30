\name{upmx}
\alias{upmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Uniform Partial Mapped Crossover
}
\description{
Uniform Partial Mapped Crossover (UPMX) is a crossover operator for permutation encoded chromosomes. Parent1 is cloned into Offspring1. A random point v1 is chosen. The gene at point v1 in Parent2 is determined. The v2 point carrying this gene is determined in Offspring1. The genes at v1 and v2 are swapped. These processes are repeated k times (Migkikh et.al., 1996).
}
\usage{
upmx(x1, x2, cxon, ...)
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
Migkikh, V.V., Topchy, A.A., Kureichick, V.M. and Tetelbaum, A.Y. (1996). Combined Genetic and local search algorithm for the quadratic assignment problem. In \emph{Proc. of IC on Evolutionary Computation and Its Applications, EvCA}, 96, 335-341.
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
upmx(parent1, parent2)
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
