\name{ammiBayes.mean.plot}
\alias{ammiBayes.mean.plot}

\title{
  Plot ammiBayes object
}

\description{
  Plot the means for the ammiBayes object
}

\usage{
ammiBayes.mean.plot(model, pars.gen=NULL, pars.env=NULL,
              gen.labels=NULL, env.labels=NULL, 
              col.text.gen="darkgreen", col.text.env="red",
              ylim=NULL, xlim=NULL, cex.env=1, cex.gen=1,
              xlab,	ylab, col.grid="grey", lty.grid=2, lwd.grid=1)
}

\arguments{
  \item{model}{An object of the ammiBayes class}
  \item{pars.gen}{An optional character vector of genotype names. If pars is omitted all genotypes are included.}
  \item{pars.env}{An optional character vector of environment names. If pars is omitted all environments are included.}
  \item{gen.labels}{Optional vector for the name of the genotypes}
  \item{env.labels}{Optional vector for the name of the environments}
  \item{col.text.gen}{Define the color of genotype names}  
  \item{col.text.env}{Define the color of environment names}  
  \item{ylim}{Define the limites applied to the y-axis}    
  \item{xlim}{Define the limites applied to the x-axis}    
  \item{cex.env}{Scale for the font size of the environment names. Default is 1}    
  \item{cex.gen}{Scale for the font size of the genotype names. Default is 1}    
  \item{xlab}{Label for the x-axis}    
  \item{ylab}{Label for the y-axis}    
  \item{col.grid}{Define the color for the grid. Default is "grey"}    
  \item{lty.grid}{Line type of grid}
  \item{lwd.grid}{Line width of grid}        
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

ammiBayes.mean.plot(model)
}
\keyword{package}

