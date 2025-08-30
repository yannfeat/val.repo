\name{initialize}
\alias{initialize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize function
}
\description{
The initialize function is a function that wraps various initialization functions.
}
\usage{
initialize(initfunc, n, m, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initfunc}{Initialization function}
  \item{n}{Population size}
  \item{m}{Chromosome length (number of variables)}
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
  \code{\link{initbin}},
  \code{\link{initval}},
  \code{\link{initperm}},
  \code{\link{initnorm}}
}
\examples{
initpop = initialize(initfunc=initbin, n=6, m=4) 
initpop
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
