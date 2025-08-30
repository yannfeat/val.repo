\name{calcM}
\alias{calcM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the number of bits in the binary representation of the integer vector
}
\description{
The function CalcM calculates the number of bits in the binary representation of the integer vector
}
\usage{
calcM(ub, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ub}{A vector containing upper bounds}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
This function uses the upper bounds of the integer vector to calculate the number of bits in the binary representation of an integer vector.
}
\value{
A vector of the numbers of bits for binary representation of an integer vector.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{encode4int}}
}
\examples{
ub = c(10, 10, 10)
calcM(ub)
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
