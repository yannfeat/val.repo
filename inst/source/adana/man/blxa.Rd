\name{blxa}
\alias{blxa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Blended Crossover (BLX-\eqn{\alpha}) 
}
\description{
Eshelman and Schaffer (1993) proposed an algorithm called Blended-\eqn{\alpha} Crossover (BLX-\eqn{\alpha}) by introducing the concept of interval scheme to be applied in real-valued problems (Takahashi & Kita, 2001).
}
\usage{
blxa(x1, x2, cxon, ...)
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
Eshelman, L.J. and Schaffer, J.D. (1993). Real-coded genetic algorithms and interval schemata. In \emph{Foundations of Genetic Algorithms}, Vol. 2, pp. 187-202, Elsevier.

Takahashi, M. and Kita, H. (2001). A crossover operator using independent component analysis for real-coded genetic algorithms. In \emph{Proc. of the 2001 Cong. on Evolutionary Computation (IEEE Cat.No. 01TH8546)}, Vol. 1, pp. 643-649. IEEE.  
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
  \code{\link{ebx}},
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
ebeveyn1 = c(1.1, 1.6, 0.0, 1.1, 1.4, 1.2)
ebeveyn2 = c(1.2, 0.0, 0.0, 1.5, 1.2, 1.4)
blxa(ebeveyn1, ebeveyn2, cxon=3)
}
