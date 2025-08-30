\name{maxone1}
\alias{maxone1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MAXONE1 fitness function
}
\description{
Fitness function that calculates the number of 1s in each individual
}
\usage{
maxone1(x, ...)
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
  \code{\link{maxone}}, 
  \code{\link{maxone2}}, 
  \code{\link{minone}}
}
\examples{
C2 = c(1, 1, 1, 0, 1, 0, 0, 0)
maxone1(C2)
C3 = c(1, 1, 1, 1, 1, 1, 1, 1)
maxone1(C3)
}