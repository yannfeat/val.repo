\name{tensile}
\alias{tensile}
\docType{data}
\title{
The Tensile Strength Experiment
}
\description{
An engineer wants to find out if the cotton weight percentage in a synthetic fiber effects the tensile strength. Towards this, the cotton weight percentage is fixed at 5 different levels of 15, 20, 25, 30, and 35. Each level of the percentage is assigned 5 experimental units and the tensile strength is measured on each of them. The randomization is specified in the \code{Run_Number} column. The goal of the engineer is to investigate if the tensile strength is same across the cotton weight percentage. 
}
\usage{data(tensile)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{Test_Sequence}}{the test sequence}
    \item{\code{Run_Number}}{the run number}
    \item{\code{CWP}}{cotton weight percentage}
    \item{\code{Tensile_Strength}}{the tensile strength}
  }
}
\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(tensile)
tensile$CWP <- as.factor(tensile$CWP)
tensile_aov <- aov(Tensile_Strength~CWP, data=tensile)
summary(tensile_aov)
model.matrix(tensile_aov)
}
\keyword{completely randomized design}
