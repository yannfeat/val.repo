\name{ineq.md}
\alias{ineq.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Three Inequality Measures for a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{ This function uses Monte Carlo methods to estimate an the mean logarithmic deviation, the Theil Index and the Gini Coefficient for a mixture of two continuous income distributions and a point mass for zero-incomes.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ineq.md(n, dist1, dist2, theta, 
p0, p1, p2, dist.para.table, zero.approx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ sample size used to estimate the gini coefficient.
%%     ~~Describe \code{n} here~~
}
  \item{dist1}{character string with the name of the first continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
%%     ~~Describe \code{dist1} here~~
}
  \item{dist2}{character string with the name of the second continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
%%     ~~Describe \code{dist2} here~~
}
  \item{theta}{vector with the parameters of dist1 and dist2. Order must be the same as in the functions for the distributions.
%%     ~~Describe \code{theta} here~~
}
  \item{p0}{scalar with probability mass for the point mass.
%%     ~~Describe \code{p0} here~~
}
  \item{p1}{scalar with probability mass for dist1.
%%     ~~Describe \code{p1} here~~
}
  \item{p2}{ scalar with probability mass for dist2.
%%     ~~Describe \code{p2} here~~
}
  \item{dist.para.table}{a table of the same form as \code{\link{dist.para.t}} with distribution name, function name and number of parameters.
%%     ~~Describe \code{dist.para.table} here~~
}
  \item{zero.approx}{ a scalar which replaces zero-incomes (and negative incomes), such that entropy measures involving a logarithm return finite values.
%%     ~~Describe \code{dist.para.table} here~~
}
}
\value{
\item{MLD}{the estimated mean logarithmic deviation.}
\item{Theil}{the estimated Theil index.}
\item{Gini}{the estimated Gini coefficient.}
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

\seealso{ \code{\link{dist.para.t}}, \code{\link{gini}}, \code{\link{entropy}} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
 
theta<-c(0,1,5,2)
x<- c(rgamma(500,2,1),rgamma(500,5,2))
entropy(x,0)
entropy(x,1)
gini(x)$Gini
data(dist.para.t)
im<-ineq.md(100,"gamma","gamma",theta,0,0.5,0.5,dist.para.t,zero.approx=1)
im$MLD
im$Theil
im$Gini
}