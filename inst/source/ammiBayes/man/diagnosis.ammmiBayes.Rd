\name{diagnosis.ammiBayes}
\alias{diagnosis.ammiBayes}

\title{
  Bayesian AMMI for ordinal data
}

\description{
    Extract the MCMC chain for diagnosis
}

\usage{
diagnosis.ammiBayes(x, pars=NULL)
}

\arguments{
  \item{x}{An object of class ammiBayes}
  \item{pars}{It should be set, such as "Genotype", "Rep", "L", "Gen.PC1", "Gen.PC2", "Env.PC1", "Env.PC2", "Comp.var". See details }
}

\details{
    The output is compatible for diagnosis with the \code{coda} and \code{bayesplot} packages. 
    Examples can be seen on the website: \code{\link[bayesplot]{bayesplot}}
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
\donttest{

# Not run
library(ammiBayes)
library(bayesplot)
library(ggpubr)

data(ammiData)

Env  <- factor(ammiData$amb)
Rep <- factor(ammiData$rep)
Gen  <- factor(ammiData$gen)
Y  <- ammiData$prod

model <- ammiBayes(Y=Y, Gen=Gen, Env=Env, Rep=Rep, iter=1000, burn=10, jump=2, chains=2)

gen.diagnosis <- diagnosis.ammiBayes(model, pars="Genotype")


mcmc_trace(gen.diagnosis)
mcmc_dens_overlay(gen.diagnosis)
mcmc_areas(gen.diagnosis)

dens <- bayesplot::mcmc_dens_overlay(gen.diagnosis)
trac <- bayesplot::mcmc_trace(gen.diagnosis, facet_args=list(ncol=1))

ggpubr::ggarrange(trac,dens, common.legend=TRUE)


}}

\keyword{package}

