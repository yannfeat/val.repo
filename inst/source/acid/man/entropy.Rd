\name{entropy}
\alias{entropy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Measures of the Generalised Entropy Family
%%  ~~function to do ... ~~
}
\description{ This function computes the Measures of the Generalised Entropy Family for a vector of observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
entropy(x, alpha = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{alpha}{ the parameter for the generalised entropy family of measures, denoted by alpha by Cowell (2000). Note that this parameter notation differs from the notation used in the ineq package.
%%     ~~Describe \code{parameter} here~~
}
}

\value{ returns the entropy measure.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[ineq]{ineq}}
}
\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
entropy(x)
}