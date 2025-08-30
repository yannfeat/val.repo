\name{hgaoptim}
\alias{hgaoptim}
\title{
GA + optim hybridization function
}
\description{
This function allows GA to hybridize with methods in the optim general-purpose optimization function for n-variable problems in R's basic \emph{stats} package (R Core Team, 2021).
}
\usage{
hgaoptim(genpop, fitfunc, hgaparams,
                    hgaftype, hgans, ...)
}
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
R Core Team. (2021). R: A language and environmental for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/. 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{hgaoptimx}},
 \code{\link{hgaroi}}
}
\examples{
hgaparams = list(method="Nelder-Mead", poptim=0.05, pressel=0.5,
  control = list(fnscale=1, maxit=100))
n = 5                   # Size of population 
m = 2                   # Number of variables
lb = c(-5.12, -5.12)    # Lower bounds for sample data
ub = c(5.12, 5.12)      # Upper bounds for sample data
genpop = initval(n, m, lb=lb, ub=ub) # Sample population
fitfunc = function(x, ...) 2*(x[1]-1)^2 + 5*(x[2]-2)^2 + 10
fitvals = evaluate(fitfunc, genpop[,1:m])
genpop[,"fitval"]=fitvals
genpop
newpop = hgaoptim(genpop, fitfunc, hgaparams, hgaftype="r", hgans=3)
newpop
}

