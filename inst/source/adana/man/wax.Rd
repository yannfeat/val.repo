\name{wax}
\alias{wax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Whole Arithmetic Crossover
}
\description{
New offspring are produced by applying an arithmetic mean to all of the parents' chromosomes. (Davis, 1985; Back et.al, 1991; Michalewicz & Janikov, 1991; Michalewicz, 1992; Michalewicz, 1995).
}
\usage{
wax(x1, x2, cxon, cxalfa, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{A vector. It contains the chromosomal information of parent-1.}
  \item{x2}{A vector. It contains the chromosomal information of parent-2.}
  \item{cxon}{Number of offspring to be generated as a result of crossover}
  \item{cxalfa}{Alpha value. If no value is entered, it is randomly selected by the function in the range [0,1].}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A matrix containing the generated offsprings.
}
\references{
Davis, L. (1985). Aplaying adaptive algorithms to epistatics domains. In \emph{Proc. of the Int. Joint Conf. on Artificial Intellengence}, Vol. 85, pp. 162-164.

Back, T., Hoffmeister, F. and Schwefel, H.P. (1991). A survey of evolution strategies. In \emph{Proc. of the 4th Int. Conf. on Genetic Algorithms}, pp. 2-9. Morgan Kaufmann.

Michalewicz, Z. and Janikov, S.J. (1991). Genetic algorithms for numerical optimization. \emph{Statistics and Computing}, 1(2), 75-91.

Michalewicz, Z. (1992). \emph{Genetic algorithms + data structures = evolution programs}. Berlin-Heidelberg: Springer Verlag. 

Michalewicz, Z. (1995). Genetic algorithms, numerical optimization and constraints. In \emph{Proc. of the 4th Int. Conf. on Genetic Algorithms}. pp. 151-158. Morgan Kaufmann. 
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
  \code{\link{pbx2}},
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}
}
\examples{
parent1 = c(1.1, 1.6, 0.0, 1.1, 1.4, 1.2)
parent2 = c(1.2, 0.0, 0.0, 1.5, 1.2, 1.4)
wax(parent1, parent2)
}

