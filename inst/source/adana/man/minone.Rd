\name{minone}
\alias{minone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
minone fitness function 
}
\description{
Calculates the inverse of the sum of each row of a matrix or data frame.
}
\usage{
minone(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix or a data frame}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  A vector includes the inverse of the sum of each row in a matrix or data frame
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{maxone}}, 
  \code{\link{maxone1}}, 
  \code{\link{maxone2}}
}
\examples{
binmat = matrix(nrow=5, ncol=8, byrow=TRUE, c(
1, 0, 1, 0, 1, 1, 1, 0,
1, 1, 1, 0, 1, 0, 0, 0,
1, 1, 1, 1, 1, 1, 1, 1,
0, 1, 0, 1, 0, 1, 1, 1,
0, 0, 0, 0, 0, 0, 0, 0
))
rownames(binmat) = paste0("C",1:5)
minone(binmat)
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
