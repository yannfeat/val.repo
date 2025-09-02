\name{predict.ammiBayes}
\alias{predict.ammiBayes}

\title{
  Bayesian AMMI for continuous data
}

\description{
    Extract predict values and HPD interval
}

\usage{
\method{predict}{ammiBayes}(object, prob=0.95, ...)
}

\arguments{
  \item{object}{An object of class ammiBayes}
  \item{prob}{Probability for HPD interval. Default is 0.95}
  \item{\dots}{Potential further arguments (required by generic).}  
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


\examples{

library(ammiBayes)
data(ammiData)

Env  <- factor(ammiData$amb)
Rep <- factor(ammiData$rep)
Gen  <- factor(ammiData$gen)
Y  <- ammiData$prod

model <- ammiBayes(Y=Y, Gen=Gen, Env=Env, Rep=Rep, iter=10, burn=1, jump=2, chains=2)

predict(model)
}
\keyword{package}

