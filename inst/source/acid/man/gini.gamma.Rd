\name{gini.gamma}
\alias{gini.gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gini Coefficient for the Gamma Distribution
%%  ~~function to do ... ~~
}
\description{ This function computes the Gini coefficient for the gamma distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini.gamma(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ the shape parameter p of the gamma distribution as defined by Kleiber and Kotz (2003).
%%     ~~Describe \code{p} here~~
}
}
\value{ returns the Gini coefficient.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.

Kleiber, C. and Kotz, S. (2003): Statistical Size Distributions in Economics and Actuarial Sciences, Wiley, Hoboken.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}
\seealso{ \code{\link{gini}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
shape.test <- 5
scale.test  <- 50000
y <- rgamma(10000,shape=shape.test,scale=scale.test)
gini(y)
gini.gamma(shape.test)

}
