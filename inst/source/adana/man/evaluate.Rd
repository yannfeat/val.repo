\name{evaluate}
\alias{evaluate}
\title{
Calculate the fitness values of population
}
\description{
Calculates the fitness value of a population using the fitness function given with the fitfunc argument.
}
\usage{
evaluate(fitfunc, population, objective, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitfunc}{Fitness function}
  \item{population}{Population matrix}
  \item{objective}{\dQuote{max} or \dQuote{min}}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  A vector of fitness values for each induvidual in population.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}
\examples{
population = initbin()
head(population, 5)
m = ncol(population)-2
fitvals = evaluate(maxone, population[,1:m], objective="max")
head(fitvals, 5)
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
