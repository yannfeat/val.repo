\name{initperm}
\alias{initperm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation coded initialization
}
\description{
This function generates an initial population when each variable of the chromosomes is desired to be encoded on a rank scale or permutation.
}
\usage{
initperm(n, permset, prevpop, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Population size}
  \item{permset}{A vector of permutation set}
  \item{prevpop}{Matrix of solutions used in heuristic and hybrid initialization}
  \item{type}{Type of output matrix}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
Unlike other initialization function inputs, the initperm function has an argument called permset. This argument is a vector containing permutation set values. The permutation set can contain numbers or letters. In the initial population, each variable randomly takes any value from this set, but there cannot be two of the same value in a chromosome.
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
  \code{\link{initbin}},
  \code{\link{initval}},
  \code{\link{initnorm}},
  \code{\link{initialize}}
}
\examples{
n = 20   #Population size (number of chromosemes)
m = 6    #number of Variables
lb = c(10, 2, 5, 100, 50, 25)
ub = c(40, 8, 20, 500, 250, 90)
population = initval(n, m, lb=lb, ub=ub, nmode="integer")
tail(population, 3)
}

