\name{cross}
\alias{cross}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crossover
}
\description{
It is a wrapper function that calls crossover operators from a single function.
}
\usage{
cross(crossfunc, matpool, cxon, cxpc, gatype, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{crossfunc}{The name of the crossover operator}
  \item{matpool}{A matrix. Mating pool containing selected individuals.}
  \item{cxon}{Number of offspring to be generated as a result of crossover}
  \item{cxpc}{Crossover Ratio. Default value is 0.95}
  \item{gatype}{Indicates the GA type. "gga" is assigned for generational refresh, and "ssga" for steady-state refresh.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A matrix containing the generated offsprings.
}
\references{
Cebeci, Z. (2021). R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari, 535 p. Ankara:Nobel Akademik Yayincilik.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{px1}},
  \code{\link{kpx}},
  \code{\link{sc}},
  \code{\link{rsc}},
  \code{\link{hux}},
  \code{\link{ux}},
  \code{\link{ux2}},
  \code{\link{mx}},
  \code{\link{rrc}},
  \code{\link{disc}},
  \code{\link{atc}},
  \code{\link{cpc}},
  \code{\link{eclc}},
  \code{\link{raoc}},
  \code{\link{dc}},
  \code{\link{ax}},
  \code{\link{hc}},
  \code{\link{sax}},
  \code{\link{wax}},
  \code{\link{lax}},
  \code{\link{bx}},
  \code{\link{ebx}},
  \code{\link{blxa}},
  \code{\link{blxab}},
  \code{\link{lapx}},
  \code{\link{elx}},
  \code{\link{geomx}},
  \code{\link{spherex}},
  \code{\link{pmx}},
  \code{\link{mpmx}},
  \code{\link{upmx}},
  \code{\link{ox}},
  \code{\link{ox2}},
  \code{\link{mpx}},
  \code{\link{erx}},
  \code{\link{pbx}},
  \code{\link{pbx2}},
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}
}
\examples{
genpop = initbin(12,8)                             #Initial population
m = ncol(genpop)-2                                 #Number of Gene
sumx = function(x, ...) (sum(x))                   #Fitness Function
fitvals = evaluate(fitfunc=sumx, genpop[,1:m])     #Fitness Values
genpop[,"fitval"] = fitvals
selidx = select(selfunc=selrws, fitvals)           #Selection of Parents
matpool = genpop[selidx,]                          #Mating Pool
offsprings = cross(crossfunc=px1, matpool=matpool, #Crossing
 cxon=2, cxpc=0.8, gatype="gga")
offsprings
offsprings = cross(crossfunc=kpx, matpool=matpool,
  cxon=2, cxpc=0.8, gatype="ssga", cxps=0.5, cxk=2)
offsprings
}
