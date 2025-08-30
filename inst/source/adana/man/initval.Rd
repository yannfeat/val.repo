\name{initval}
\alias{initval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Value encoded initialization
}
\description{
Initialize the population with integer or real numbers
}
\usage{
initval(n, m, prevpop, lb, ub, nmode="real", type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Population size}
  \item{m}{Chromosome length (number of variables)}
  \item{prevpop}{Matrix of solutions used in heuristic and hybrid initialization}
  \item{lb}{Lower bound of each variables}
  \item{ub}{Upper bound of each variables}
  \item{nmode}{Type of variables (\dQuote{integer} or \dQuote{real})}  
  \item{type}{Type of output matrix}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
With this function, populations are initialized with integer and/or real numbers depending on the GA problem. In this case, the value type must be known. Furthermore, the lower and upper bound values for each variable must be known. If desired, heuristic or mixed initialization can be done with the \code{prevpop} argument.
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
  \code{\link{initperm}},
  \code{\link{initnorm}},
  \code{\link{initialize}}
  }
\examples{
n = 15  #Population size (number of chromosemes)
m = 4   #number of Variables
population = initval(n, m)
head(population, 3)
tail(population, 3)
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
