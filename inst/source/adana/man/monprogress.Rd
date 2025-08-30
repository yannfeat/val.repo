\name{monprogress}
\alias{monprogress}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Monitor Fitness Value Progress
}
\description{
Monprogress function performs by creating a line plot of the best fitness value found across generations.
}
\usage{
monprogress(g, genfits, objective, ...)
}

\arguments{
  \item{g}{Generation number}
  \item{genfits}{A matrix for fitness values}
  \item{objective}{Type of optimization. "min" or "max"}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\author{
Zeynel Cebeci & Erkut Tekeli
}
\value{No return value, called for the side effect of drawing a plot.}
\seealso{
 \code{\link{show}}
}
\examples{
n = 100
genfits = matrix(NA, nrow=n, ncol=5)
genfits[1,3] = 50
objective = "max"
for(i in 1:(n-1)){
  g=i
  monprogress(g=g, genfits=genfits, objective=objective)
  genfits[g+1, 3] = genfits[g, 3] + runif(1, -2, 5)
}
}
