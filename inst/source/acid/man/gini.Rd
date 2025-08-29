\name{gini}
\alias{gini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gini Coefficient
%%  ~~function to do ... ~~
}
\description{ This function computes the Gini coefficient for a vector of observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
}

\value{ 
\item{Gini }{the Gini coefficient for the sample.}
\item{bcGini }{the bias-corrected Gini coefficient for the sample.}
}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 87-166, Elsevier, Amsterdam.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[ineq]{ineq}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
gini(x)
}