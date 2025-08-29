\name{atkinson.md}
\alias{atkinson.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Atkinson Index for a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{  This function uses Monte Carlo methods to estimate the Atkinson index for a mixture of two continuous income distributions and a point mass for zero-incomes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atkinson.md(n, epsilon = 1, dist1, dist2, theta, p0, p1, p2, 
dist.para.table, zero.approx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ sample size used to estimate the Atkinson index.
%%     ~~Describe \code{n} here~~
}
  \item{epsilon}{  inequality aversion parameter as denoted by Atkinson (1970). The default is epsilon=1. 
%%     ~~Describe \code{epsilon} here~~
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
  \item{zero.approx}{ a scalar which replaces zero-incomes, such that the Atkinson index involving a logarithm return finite values.
}
}
\value{
\item{AIM}{the selected Atkinson inequality measure.}
\item{epsilon}{the inequality aversion parameter used.}
\item{y}{a vector with the simulated incomes to estimate the entropy measure.}
\item{y2}{a vector with the zero-replaced simulated incomes to estimate the entropy measure.}
\item{zero.replace}{a logical vector indicating whether a zero has been replaced.}
\item{stat}{a vector with the simulated group the observation was chosen from. 0 is the point mass, 1 dist1 and 2 dist2.}

}
\references{ Atkinson, A.B. (1970): On the Measurment of Inequality, in: Journal of Economic Theory, Vol. 2(3), pp. 244-263.

}
\author{Alexander Sohn
%%  ~~who you are~~
}

\seealso{ \code{\link[ineq]{ineq}} , \code{\link{atkinson}} , \code{\link{atkinson.den}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
theta<-c(2,1,5,2)
x<- c(rgamma(50000,2,1),rgamma(50000,5,2))
para<-1

data(dist.para.t)
atkinson.md(10000,para,"gamma","gamma",theta,0,0.5,0.5,dist.para.t,zero.approx=1)$AIM
atkinson(x,1)

}