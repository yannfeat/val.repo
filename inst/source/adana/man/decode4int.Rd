\name{decode4int}
\alias{decode4int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert binary vectors to integer vectors
}
\description{
The function decode4int converts each element in a given binary vector to a integer number.
}
\usage{
decode4int(x, M, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector containing binary numbers}
  \item{M}{A vector containing the number of bits of each binary in the x.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
This function converts each element in the binary vector passed with the x argument to an integer. The M argument refers to the number of bits of each binary in the x vector.
}
\value{
A vector of integer for input binary vector 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{encode4int}}
}
\examples{
binmat = c(0,0,1,1,1,0,0,1,0,0,1,0)
M = c(4,4,4)
intmat = decode4int(binmat, M=M)
intmat
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
