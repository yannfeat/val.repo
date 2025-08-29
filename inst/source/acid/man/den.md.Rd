\name{den.md}
\alias{den.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Density for a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{ This function computes the p-value for a mixture of two continuous income distributions and a point mass for zero-incomes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
den.md(y, dist1, dist2, theta, p0, p1, p2, dist.para.table)
}
\arguments{
  \item{y}{ a vector with incomes. If a zero income is included, it must be the first element. 
}
  \item{dist1}{character string with the name of the first continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{dist2}{character string with the name of the second continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{theta}{vector with the parameters of dist1 and dist2. Order must be the same as in the functions for the distributions.
}
  \item{p0}{scalar with probability mass for the point mass.
}
  \item{p1}{scalar with probability mass for dist1.
}
  \item{p2}{scalar with probability mass for dist2.
}
  \item{dist.para.table}{a table of the same form as \code{\link{dist.para.t}} with distribution name, function name and number of parameters.
}
}
\value{ returns the density for given values of y.
}
\author{ Alexander Sohn
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{  \code{\link{ysample.md}}, \code{\link{pval.md}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(dist.para.t)
ygrid<-seq(0,20,by=0.1)#c(seq(0,1e5,by=100),seq(1.1e5,1e6,by=100000))
theta<-c(5,1,10,1.5)
p0<-0.2
p1<-0.3
p2<-0.5
n <-100000
y.sim <- ysample.md(n, "norm", "norm", theta, p0, p1, p2, dist.para.t)
den<-den.md(ygrid,"norm", "norm", theta, 
              p0, p1, p2, dist.para.table=dist.para.t)
hist(y.sim,freq=FALSE)
#hist(y.sim,breaks=c(seq(0,1e5,by=100),seq(1.1e5,1e6,by=100000)),xlim=c(0,2e4),ylim=c(0,0.001))
lines(ygrid,den,col=2)
}