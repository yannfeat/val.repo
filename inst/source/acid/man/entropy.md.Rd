\name{entropy.md}
\alias{entropy.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Generalised Entropy Measure for a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{ This function uses Monte Carlo methods to estimate an entropy measure for a mixture of two continuous income distributions and a point mass for zero-incomes.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
entropy.md(n, alpha = 1, dist1, dist2, theta, 
p0, p1, p2, dist.para.table, zero.approx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ sample size used to estimate the entropy measure.
%%     ~~Describe \code{n} here~~
}
  \item{alpha}{ the parameter for the generalised entropy family of measures, denoted by alpha by Cowell (2000). Note that this parameter notation differs from the notation used in the ineq package.
%%     ~~Describe \code{alpha} here~~
}
  \item{dist1}{ character string with the name of the first continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{dist2}{ character string with the name of the second continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{theta}{ vector with the parameters of dist1 and dist2. Order must be the same as in the functions for the distributions.
}
  \item{p0}{  scalar with probability mass for the point mass.
}
  \item{p1}{ scalar with probability mass for dist1.
}
  \item{p2}{ scalar with probability mass for dist2.
}
  \item{dist.para.table}{ a table of the same form as \code{\link{dist.para.t}} with distribution name, function name and number of parameters.
}
  \item{zero.approx}{ a scalar which replaces zero-incomes (and negative incomes), such that entropy measures involving a logarithm return finite values.
}
}
\value{
\item{entropy}{the estimated entropy measure.}
\item{alpha}{the entropy parameter used.}
\item{y}{a vector with the simulated incomes to estimate the entropy measure.}
\item{y2}{a vector with the zero-replaced simulated incomes to estimate the entropy measure.}
\item{zero.replace}{a logical vector indicating whether a zero has been replaced.}
\item{stat}{a vector with the simulated group the observation was chosen from. 0 is the point mass, 1 dist1 and 2 dist2.}

}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 87-166, Elsevier, Amsterdam.
%% ~put references to the literature/web site here ~
}
\author{Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{dist.para.t}}, \code{\link{entropy}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{ 
theta<-c(2,1,5,2)
x<- c(rgamma(500,2,1),rgamma(500,5,2))
para<-1
entropy(x,para)
data(dist.para.t)
entropy.md(100,para,"gamma","gamma",theta,0,0.5,0.5,dist.para.t,zero.approx=1)$entropy
}