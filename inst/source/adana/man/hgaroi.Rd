\name{hgaroi}
\alias{hgaroi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GA + ROI hybridization function
}
\description{
This function allows GA to hybridize with methods in the \emph{ROI} package (Theussl et.al., 2020).
}
\usage{
hgaroi(genpop, fitfunc, hgaparams,
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
Theussl, S., Schwendinger, F. and Hornik, K. (2020). ROI: An extensible R optimization infrastructure.  \emph{Journal of Statistical Software}, 94(15), 1-64.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{hgaoptim}},
 \code{\link{hgaoptimx}}
}
\examples{
n = 5                                 # Size of population 
m = 2                                 # Number of variable
lb = c(-5.12, -5.12)                  # Lower bounds of sample data
ub = c(5.12, 5.12)                    # Upper bounds of sample data
hgaparams = list(method="L-BFGS-B", 
  poptim=0.05, pressel=0.5,
  lower=lb, upper=ub,
  control=list(maxit=100))
genpop = initval(n, m, lb=lb, ub=ub)  # Sample population
fitfunc = function(x, ...) 2*(x[1]-1)^2 + 5*(x[2]-2)^2 + 10
fitvals = evaluate(fitfunc, genpop[,1:m])
genpop[,"fitval"]=fitvals
genpop
genpop = hgaroi(genpop, fitfunc, hgaparams, 
  hgaftype="r", hgans=3)
genpop
}
