\name{weighted.gini}
\alias{weighted.gini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Gini Coefficient
%%  ~~function to do ... ~~
}
\description{ This function computes the Gini coefficient for a vector of observations with corresponding weights.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weighted.gini(x, w = NULL)
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
\value{ returns the Gini coefficient.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.
%% ~put references to the literature/web site here ~
}
\author{Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[ineq]{ineq}}
}
\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
w <- sample(1:2,length(x),replace=TRUE)
weighted.gini(x,w)
}