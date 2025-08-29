\name{weighted.atkinson}
\alias{weighted.atkinson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Atkinson Inequality Index
%%  ~~function to do ... ~~
}
\description{  This function computes the Atkinson inequality index for a vector of observations with corresponding weights.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weighted.atkinson(x, w = NULL, epsilon = 1, wscale = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{w}{  a vector of weights. If 
%%     ~~Describe \code{w} here~~
}
  \item{epsilon}{ inequality aversion parameter as denoted by Atkinson (1970). The default is epsilon=1. 
%%     ~~Describe \code{epsilon} here~~
}
  \item{wscale}{ a scale by which the weights are adjusted such that can be rounded to natural numbers. 
%%     ~~Describe \code{wscale} here~~
}
}
\value{ returns the selected Atkinson inequality index.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{  Atkinson, A.B. (1970): On the Measurment of Inequality, in: Journal of Economic Theory, Vol. 2(3), pp. 244-263.

}
\author{ Alexander Sohn
%%  ~~who you are~~
}

\seealso{ \code{\link[ineq]{ineq}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
w <- sample(1:2,length(x),replace=TRUE)
weighted.atkinson(x,w)
}