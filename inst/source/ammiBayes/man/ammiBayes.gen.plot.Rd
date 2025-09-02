\name{ammiBayes.gen.plot}
\alias{ammiBayes.gen.plot}

\title{
  Plot genotype effects from ammiBayes object
}

\description{
  Plot the posterior mean for an ammiBayes object
}

\usage{
ammiBayes.gen.plot(x, lwd=1, lty=1, pch=18, method="bars",
 				   col=NULL, ylim=NULL, draw.mean=TRUE, col.mean="red",
				   lty.mean=2, xlab="Genotype", ylab=NULL, gen.names=NULL)
}

\arguments{
  \item{x}{An object from gen.effects function.}
  \item{lwd}{A line width, a positive number, default is 1.}
  \item{lty}{The line type. Default is 1.}
  \item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting points.}
  \item{method}{Defaults to \code{"bars"} to draw error-bar type plots. See \code{\link[Hmisc]{panel.xYplot}}.}
  \item{col}{Define the color of genotype bands. See \code{\link[Hmisc]{xYplot}}.}  
  \item{ylim}{A numeric vector of length 2 giving minimum and maximum for the y-axis.}
  \item{draw.mean}{If TRUE, a line representing the average of all genotypes is plotted on the graph.}
  \item{col.mean}{Define the color for the average of all genotypes.}
  \item{lty.mean}{Line type for the average of all genotypes.}  
  \item{xlab}{Label for the x-axis.}    
  \item{ylab}{Label for the y-axis.}    
  \item{gen.names}{Define the names of genotypes on the x-axis. By default are the levels of the Genotypes.}        
}

\references{
OLIVEIRA,L.A.; SILVA, C. P.; NUVUNGA, J. J.; SILVA, A. Q.; BALESTRE, M. Credible Intervals for Scores in the AMMI with Random Effects for Genotype. Crop Science, v. 55, p. 465-476, 2015.
doi:  https://doi.org/10.2135/cropsci2014.05.0369

SILVA, C. P.; OLIVEIRA, L. A.; NUVUNGA, J. J.; PAMPLONA, A. K. A.; BALESTRE, M. A Bayesian Shrinkage Approach for AMMI Models. 
Plos One, v. 10, p. e0131414, 2015. doi: https://doi.org/10.1371/journal.pone.0131414.
} 



\author{
        Luciano A. Oliveira \cr
        Carlos P. Silva \cr
        Cristian T. E. Mendes \cr
        Alessandra Q. Silva \cr
        Joel J. Nuvunga \cr
        Marcio Balestre \cr
        Julio S. S. Bueno-Filho \cr
		Fabio M. Correa
}

\seealso{
  \code{\link[Hmisc]{xYplot}}
}

\examples{

library(ammiBayes)
data(ammiData)


Env  <- factor(ammiData$amb)
Rep <- factor(ammiData$rep)
Gen  <- factor(ammiData$gen)
Y  <- ammiData$prod

model <- ammiBayes(Y=Y, Gen=Gen, Env=Env, Rep=Rep, iter=10, burn=1, jump=2, chains=2)

genot.effects <- gen.effects(model)


ammiBayes.gen.plot(genot.effects)
}
\keyword{package}

