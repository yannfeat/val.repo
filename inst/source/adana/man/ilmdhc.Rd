\name{ilmdhc}
\alias{ilmdhc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ILM/DHC adaptation function
}
\description{
ILM/DHC is an adaptive function with an increasing low mutation rate (ILM) and a decreasing high crossover rate (DHC) as the generation progresses (Hassanat et.al., 2019).
}
\usage{
ilmdhc(g, gmax, ...)
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
Hassanat, A., Almohammadi, K., Alkafaween, E., Abunawas, E., Hammouri, A. and Prasath, V.B. (2019). Choosing mutation and crossover ratios for genetic algorithm: A review with a new dynamic approach. \emph{Information}, 10(12), 390.   
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{fixpcmut}},
 \code{\link{adana1}},
 \code{\link{adana2}},
 \code{\link{leitingzhi}},
 \code{\link{adana3}}
}
\examples{
N = 50
gmax = 1000
g = c(1, 10, 50, 100, 250, 500, 750, gmax)
pc = ilmdhc(g=g, gmax=gmax)$pc
pc
nc = round(pc*N)
nc
pm = ilmdhc(g=g, gmax=gmax)$pm
pm
nm = round(pm*N)
nm
nm = ifelse (!nm, 1, nm)
nm
plot(pm, type="l", col=4, lwd=2, lty=1, xaxt="n", ylab="Ratio", xlab="Generation")
lines(pc, type="l", col=2, lwd=2, lty=2)
legend("top", inset=.02, c("pm","pc"), col=c(4,2), lty=c(1,2), horiz=TRUE, cex=0.8)
axis(1, at=1:length(g),labels=g, col.axis="red", las=2)
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
