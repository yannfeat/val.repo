\name{atkinson}
\alias{atkinson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Atkinson Inequality Index
%%  ~~function to do ... ~~
}
\description{  This function computes the Atkinson inequality index for a vector of observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atkinson(x, epsilon = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{  a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{epsilon}{ inequality aversion parameter as denoted by Atkinson (1970). The default is epsilon=1. 
%%     ~~Describe \code{epsilon} here~~
}
}
\value{  returns the selected Atkinson inequality index.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ Atkinson, A.B. (1970): On the Measurment of Inequality, in: Journal of Economic Theory, Vol. 2(3), pp. 244-263.
%% ~put references to the literature/web site here ~
}
\author{  Alexander Sohn
%%  ~~who you are~~
}

\seealso{ \code{\link[ineq]{ineq}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
atkinson(x)
}