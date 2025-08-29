\name{weighted.coeffvar}
\alias{weighted.coeffvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Coefficient of Variation
%%  ~~function to do ... ~~
}
\description{ This function computes the Coefficient of Variation for a vector of observations and corresponding weights.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weighted.coeffvar(x, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{w}{ a vector of weights.
%%     ~~Describe \code{w} here~~
}
}
\value{
\item{cv }{returns the coefficient of variation without bias correction.}
\item{bccv }{returns the coefficient of variation with bias correction.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Atkinson, A.B. and Bourguignon, F. (2000): Income Distribution and Economics, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.
}
\author{Alexander Sohn
}

\section{Warning }{
 Weighting is not properly accounted for in the sample adjustment of bccv!
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[ineq]{ineq}}
}
\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
w <- sample(1:10,length(x), replace=TRUE)
weighted.coeffvar(x,w)
}