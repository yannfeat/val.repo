\name{maxone2}
\alias{maxone2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
maxone2 fitness function
}
\description{
Calculates the sum of each row of a matrix or data frame.
}
\usage{
maxone2(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix or a data frame}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  A vector includes sum of each row in a matrix or data frame
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{maxone1}}, 
  \code{\link{maxone}}, 
  \code{\link{minone}}
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
maxone2(binmat)
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
