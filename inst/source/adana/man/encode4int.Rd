\name{encode4int}
\alias{encode4int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert integer vectors to binary vectors
}
\description{
The function encode4int converts each element in a given integer vector to a binary number.
}
\usage{
encode4int(x, M, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector containing integer numbers}
  \item{M}{A vector containing the number of bits in the binary representation of each integer variable.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
This function converts each element in the integer vector passed with the x argument to a binary number. The M argument refers to the number of bits in the binary representation of each integer variable.
}
\value{
A vector of binary representation of input vector 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{decode4int}},
  \code{\link{calcM}}
}
\examples{
n = 5
lb = c(0, 0, 0)
ub = c(10, 10, 10)
set.seed(1)
intmat = matrix(round(runif(3*n, lb, ub)), nr=n, nc=3) 
colnames(intmat) = paste0("v",1:3)
head(intmat)
M = calcM(ub)
M
binmat = matrix(NA, nrow=n, ncol=sum(M))
for(i in 1:n)
  binmat[i,] = encode4int(intmat[i,], M=M)
head(binmat)
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
