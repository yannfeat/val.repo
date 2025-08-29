\name{pval.md}
\alias{pval.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  P-Value for a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{ This function computes the p-value for a mixture of two continuous income distributions and a point mass for zero-incomes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pval.md(y, dist1, dist2, theta, p0, p1, p2, dist.para.table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector with incomes. If a zero income is included, it must be the first element. 
%%     ~~Describe \code{y} here~~
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
\value{ returns the p-value.
}
\author{ Alexander Sohn
}


\seealso{ \code{\link{ysample.md}}, \code{\link{den.md}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(dist.para.t)
ygrid<-seq(0,1e5,by=1000)
theta<-c(5,1,10,3)
p0<-0.2
p1<-0.3
p2<-0.5
n <-10000
y.sim <- ysample.md(n, "LOGNO", "LOGNO", theta, p0, p1, p2, dist.para.t)
pval<-pval.md(ygrid,"LOGNO", "LOGNO", theta, 
              p0, p1, p2, dist.para.table=dist.para.t)
mean(y.sim<=ygrid[10])
pval[10]
}