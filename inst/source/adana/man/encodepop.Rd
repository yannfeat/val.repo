\name{encodepop}
\alias{encodepop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Binary encoding of real number matrix
}
\description{
The encodepop function generates a population encoded with binary representation from a real-valued population given as a matrix.
}
\usage{
encodepop(x, lb, ub, eps, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector containing real numbers}
  \item{lb}{A vector containing lower bounds for variables}
  \item{ub}{A vector containing upper bounds for variables}
  \item{eps}{Sensitivity vector containing desired sensitivity values for each variable}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  \item{binmat}{A binary coded matrix as counterpart of real-valued input matrix}
  \item{m}{A vector containing bit length of each variable}
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{decodepop}}
  }
\examples{
lb = c(2.5, -2, 0)
ub = c(4.3, 2, 1.5)
eps = c(0.1, 1, 0.01)
#d = nchar(sub('^+','',sub("\\.",'',eps)))-1
d = grep('.', strsplit(as.character(eps), '')[[1]])-1
x = round(runif(5, lb[1],ub[1]),d[1])
y = round(runif(5, lb[2],ub[2]),d[2])
w = round(runif(5, lb[3],ub[3]),d[3])
pop = cbind(x, y, w)
pop
encpop = encodepop(pop, lb, ub, eps)
head(encpop$binmat[,1:10])
m = encpop$m
m
}

