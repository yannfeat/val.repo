\name{select}
\alias{select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select parents for the mating pool
}
\description{
The select function is a function that wraps all parent selection algorithms. It is coded for call purposes from adana main function.
}
\usage{
select(selfunc, fitvals, ns, selb, selbc,
                  selc, selk, sells, selns, selps, sels, selt, 
                  selt0, selw, selg, selgmax, fmin, reptype, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selfunc}{Name of selection function}
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{selb}{Exponent coefficent,  (\code{selb >= 0.0})}  
  \item{selbc}{Base of exponent}  
  \item{selc}{Scaling parameter}  
  \item{selk}{Power factor}  
  \item{sells}{Scaling factor}  
  \item{selns}{Number of Selection pressure}  
  \item{selps}{Percentage of Selection,  (\code{0.0 <= selps <= 1.0})}
  \item{sels}{Selection pressure,   (\code{1.0 <= sels <= 2.0})}  
  \item{selt}{Number of tournament size}  
  \item{selt0}{Number, Initial temperature}
  \item{selw}{Number, Window Size}
  \item{selg}{Current generation number}  
  \item{selgmax}{Maximum generation number}  
  \item{fmin}{The number to subtract from all fitness values.}  
  \item{reptype}{Type of Sampling, \code{TRUE : without repetitions}, \code{FALSE : with repetitions}}  
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\references{
Cebeci, Z. (2021). R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari, 535 p. Ankara:Nobel Akademik Yayincilik. 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{selrand}},
  \code{\link{selrswrp}},
  \code{\link{selrws}},
  \code{\link{selrws2}},
  \code{\link{selrss}},  
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selwscale}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},  
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selrscale2}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}},
  \code{\link{selboltour}},
  \code{\link{sellrs}},
  \code{\link{sellrs2}},
  \code{\link{sellrs3}},
  \code{\link{selnlrs}},
  \code{\link{selers}},
  \code{\link{seltrunc}}  
}
\examples{
# Create population
population = initialize(initfunc=initbin, n=10, m=8)
head(population, 5)
# Calculate fitness values
m = ncol(population)-2
fitvals = evaluate(maxone, population[,1:m])
population[,"fitval"] = fitvals
head(population, 5)
# Select parents by RWS
selidx = select(selfunc=selrws, fitvals=fitvals)
matpool = population[selidx,]
head(matpool, 5)
# Selected chromosomes
table(rownames(matpool))
}
