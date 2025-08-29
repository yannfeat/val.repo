\name{frac.ranks}
\alias{frac.ranks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fractional Ranks
%%  ~~function to do ... ~~
}
\description{ This function computes fractional ranks which are required for the S-Gini coefficient. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
frac.ranks(x, w = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector with sorted income values.
%%     ~~Describe \code{x} here~~
}
  \item{w}{ a vector of weights.
%%     ~~Describe \code{w} here~~
}
}
\value{ returns the fractional ranks.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
van Kerm, P. (2009): sgini - Generalized Gini and Concentration coefficients (with factor decomposition) in Stata', CEPS/INSTEAD, Differdange, Luxembourg. 
}
\author{
Alexander Sohn
}

\seealso{ \code{\link{sgini}}  ,  \code{\link{sgini.den}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}