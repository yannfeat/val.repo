\name{maxone}
\alias{maxone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MAXONE fitness function
}
\description{
Fitness function that calculates the number of 1s in each individual
}
\usage{
maxone(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
Number of 1s
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{maxone1}}, 
  \code{\link{maxone2}}, 
  \code{\link{minone}}
}
\examples{
C2 = c(1, 1, 1, 0, 1, 0, 0, 0)
maxone(C2)
C3 = c(1, 1, 1, 1, 1, 1, 1, 1)
maxone(C3)
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
