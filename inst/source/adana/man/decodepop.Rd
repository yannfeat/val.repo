\name{decodepop}
\alias{decodepop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert binary number matrix to real number matrix
}
\description{
The decodepop function generates  a real-valued population from a population encoded with binary representation.
}
\usage{
decodepop(x, lb, ub, m,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector containing binary numbers}
  \item{lb}{A vector containing lower bounds for variables}
  \item{ub}{A vector containing upper bounds for variables}
  \item{m}{Length for each variable}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  A real-valued matrix
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{encodepop}}
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
encpop = encodepop(pop, lb=lb, ub=ub, eps=eps)
pop = encpop$binmat 
m = encpop$m
decpop = decodepop(pop, lb=lb, ub=ub, m=m)
decpop
for(j in 1:ncol(decpop)) decpop[,j]=round(decpop[,j], d[j])
decpop
}
