\name{initbin}
\alias{initbin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize the population with binary encoding
}
\description{
The initbin function is an initialization function that can be used for binary encoding. It generates an initial population of population size n and chromosome length m.
}
\usage{
initbin(n, m, prevpop, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Population size}
  \item{m}{Chromosome length}
  \item{prevpop}{Matrix of solutions used in heuristic and hybrid initialization}
  \item{type}{Type of output matrix}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  The output matrix includes only chromosomes of initial population when \code{type=2}, otherwise The output matrix includes 
chromosomes of initial population and additional two empty columns for generation number and fitness values.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{initval}},
  \code{\link{initperm}},
  \code{\link{initnorm}},
  \code{\link{initialize}}
}
\examples{
n = 20  #Population size (number of chromosemes)
m = 5   #Number of gene (chromosome length)
population = initbin(n, m)
head(population, 4)
tail(population, 4)
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
