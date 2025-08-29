\name{weighted.moments}
\alias{weighted.moments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Moments of a Random Variable
%%  ~~function to do ... ~~
}
\description{ This functions calculates the first three moments as well as mean, standard deviation and skewness for a vector of observations with corresponding weights. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weighted.moments(x, w8 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{w8}{ a vector of weights.
%%     ~~Describe \code{w8} here~~
}
}
\value{
\item{fm}{returns the first moment.}
\item{weighted.mean}{returns the mean.}
\item{sm}{returns the second moment.}
\item{weighted.sd}{returns the uncorrected (population) standard deviation.}
\item{wtd.sd}{returns the sample-size corrected standard deviation estimate.}
\item{tm}{returns the third moment.}
\item{w.skew.SAS}{returns the skewness estimate as implemented in SAS.}
\item{w.skew.Stata}{returns the skewness estimate as implemented in Stata.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link[Hmisc]{wtd.var}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}