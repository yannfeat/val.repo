\name{initnorm}
\alias{initnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normal distribution based initialization
}
\description{
The pmean and psd arguments of this function represent the mean and standard deviation of a normally distributed population, respectively. Using these parameters, the function generates a random initial population with n individuals and m variables.~
}
\usage{
initnorm(n, m, pmean, psd, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Population size}
  \item{m}{Chromosome length (number of variables)}
  \item{pmean}{Mean of normal distribution} 
  \item{psd}{Standard deviation of normal distribution} 
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
  \code{\link{initialize}}
}
\examples{
initpop = initialize(initfunc=initnorm, n=20, m=5, 
  pmean=50, psd=5, type=2) 
head(initpop,3)
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
