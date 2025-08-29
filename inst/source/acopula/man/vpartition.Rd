\name{vpartition}
\alias{vpartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vector partitioning
}
\description{
Split a vector to subvectors of specified lengths.
}
\usage{
vpartition(x, lengths, matrixify = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector to be splitted.}
  \item{lengths}{numeric vector. Lengths of the subvectors.}
  \item{matrixify}{logical. Whether to return matrix if lengths are identical.}
}
\details{
If \code{sum(lengths)>length(x)} holds true, the extra places are filled with \code{NA}s.
}
\value{
List of numeric vectors, or a matrix if all lengths are equal.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
vpartition(1:10,c(4,5,2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ split vector }
