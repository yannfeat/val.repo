\name{hc}
\alias{hc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heuristic Crossover
}
\description{
The Heuristic Crossover (HC) operator is a conditional operator (Herrera et.al, 1998; Umbarkar & Sheth, 2005). A random r value is generated in the range [0,1]. Then if Parent2's fitness value is greater than or equal to Parent1's fitness value, the difference between them is weighted by r and added to Parent2. It is subtracted in minimization problems. This operator produces a single offspring, but due to the random value of r, repeated offspring may result in different offspring.
}
\usage{
hc(x1, x2, fitfunc, cxon, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{A vector. It contains the chromosomal information of parent-1.}
  \item{x2}{A vector. It contains the chromosomal information of parent-2.}
  \item{fitfunc}{Fitness Function}
  \item{cxon}{Number of offspring to be generated as a result of crossover}
  \item{\dots}{Further arguments passed to or from other methods.}
  }
\value{
A matrix containing the generated offsprings.
}
\references{
Herrera, F., Lozano, M. and Verdegay, J.L. (1998). Tackling real-coded genetic algorithms: Operators and tools for behavioural analysis. \emph{Artificial Intellegence Review}, 12(4), 265-319
Umbarkar, A.J. and Sheth P.D. (2015). Crossover operators in genetic algorithms: A riview, \emph{ICTACT Journal on Soft Computing}, 6(1), 1083-1092.   
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
  \code{\link{pbx2}},
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}
}
\examples{
fitfunc = function(x, ...) 2*(x[1]-1)^2 + 5*(x[2]-2)^2 + 10
parent1 = c(1.1, 1.6, 0.0, 1.1, 1.4, 1.2)
parent2 = c(1.2, 0.0, 0.0, 1.5, 1.2, 1.4)
hc(parent1, parent2, fitfunc)
}