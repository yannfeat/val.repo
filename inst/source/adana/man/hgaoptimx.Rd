\name{hgaoptimx}
\alias{hgaoptimx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GA + optimx hybridization function
}
\description{
This function allows GA to hybridize with methods in the \emph{optimx} package (Nash & Varadhan, 2011; Nash, 2014).
}
\usage{
hgaoptimx(genpop, fitfunc, hgaparams,
                     hgaftype, hgans, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genpop}{A matrix of individuals in the current population and their fitness values.}
  \item{fitfunc}{Fitness function}
  \item{hgaparams}{A list of parameters defined for use by the Optim function.}
  \item{hgaftype}{Types of fitness to transfer. \itemize{
    \item{w: individuals with the worst fitness value}
    \item{b: individuals with the best fitness value}
    \item{r: randomly selected individuals}
  }
}
  \item{hgans}{Number of individuals to be transferred to the Optim.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A matrix containing the updated population.
}
\references{
Nash, J.C. and Varadhan, R. (2011). Unified optimization algorithms to aid software system users: optimx for R. \emph{Journal of Statistical Software}, 43(9), 1-14. URL http://www.jstatsoft.org/v43/i09.
Nash, J.C. (2014). On best practice optimization methods in R.  \emph{Journal of Statistical Software}, 60(2), 1-14. URL http://www.jstatsoft.org/v60/i02.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{hgaoptim}},
 \code{\link{hgaroi}}
}
\examples{
n = 5                                # Size of population 
m = 2                                # Number of Variables
lb = c(-5.12, -5.12)                 # Lower bounds of sample data
ub = c(5.12, 5.12)                   # Upper bounds of sample data
hgaparams = list(method="L-BFGS-B", 
  poptim=0.05, pressel=0.5,
  lower=lb, upper=ub,
  control=list(maximize=FALSE, maxit=100))
genpop = initval(n, m, lb=lb, ub=ub) # Sample population
fitfunc = function(x, ...) 2*(x[1]-1)^2 + 5*(x[2]-2)^2 + 10
fitvals = evaluate(fitfunc, genpop[,1:m])
genpop[,"fitval"]=fitvals
genpop
genpop = hgaoptimx(genpop, fitfunc, hgaparams, hgaftype="r", hgans=3)
genpop
}
