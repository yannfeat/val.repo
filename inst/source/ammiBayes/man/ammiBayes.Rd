\name{ammiBayes}
\alias{ammiBayes}

\title{
  Bayesian AMMI for continuous data
}

\description{
    Run the AMMI Bayesian model for continuous data.
}

\usage{
ammiBayes(Y=Y, Gen=Gen, Env=Env, Rep=Rep, 
          iterations=3000, jump=2, burn=500,
          Var.error=0.5, Var.env=0.5, Var.gen=0.5,
          chains=2)
}

\arguments{
  \item{Y}{Response variable vector}
  \item{Gen}{Genotype effects vector. Must be defined as factor}
  \item{Env}{Environmental effects vector. Must be defined as factor} 
  \item{Rep}{Repetition vector. Must be defined as factor} 
  \item{iterations}{Total of iterations after burnin and jumo}
  \item{jump}{Jump of iterations}
  \item{burn}{Initial burn}
  \item{Var.error}{Priori for the variance of error. Default is 0.5}
  \item{Var.env}{Priori for the variance of environment. Default is 0.5}
  \item{Var.gen}{Priori for the variance of genotype. Default is 0.5}
  \item{chains}{Number of chains. See details.}
}

\details{
  The code is run in parallel for linux SO. If you are using Windows, the execution of the code will be serially. 
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

model <- ammiBayes(Y=Y, Gen=Gen, Env=Env, Rep=Rep, iter=10, 
									 burn=1, jump=2, chains=2)

summary(model)
}
\keyword{package}

