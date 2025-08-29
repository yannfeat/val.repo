\name{theil.gamma}
\alias{theil.gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Theil Index for the Gamma Distribution
%%  ~~function to do ... ~~
}
\description{  This function computes the Theil index for the gamma distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
theil.gamma(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ the shape parameter p of the gamma distribution as defined by Kleiber and Kotz (2003).
%%     ~~Describe \code{p} here~~
}
}

\value{ returns the Theil index.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{  Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.

Kleiber, C. and Kotz, S. (2003): Statistical Size Distributions in Economics and Actuarial Sciences, Wiley, Hoboken.
%% ~put references to the literature/web site here ~
}
\author{  Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{entropy}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
shape.test <- 5
scale.test  <- 50000
y <- rgamma(10000,shape=shape.test,scale=scale.test)
entropy(y,1)
theil.gamma(shape.test)

}
