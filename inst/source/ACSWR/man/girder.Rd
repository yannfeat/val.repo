\name{girder}
\alias{girder}
\docType{data}
\title{
Strength Data Set of a Girder Experiment
}
\description{
The shear strength of steel plate girders need to be modeled as a function of the four methods and nine girders. 
}
\usage{data(girder)}
\format{
  A data frame with 9 observations on the following 5 variables.
  \describe{
    \item{\code{Girder}}{The row names, varying from S1.1 to S4.2, represent the nine type of girders, \code{S1.1} \code{S1.2} \code{S2.1} \code{S2.2} \code{S3.1} \code{S3.2} \code{S4.1} \code{S4.2} \code{S5.1}}
    \item{\code{Aarau}}{one of the four methods of preparation of the steel plates}
    \item{\code{Karisruhe}}{one of the four methods of preparation of the steel plates}
    \item{\code{Lehigh}}{one of the four methods of preparation of the steel plates}
    \item{\code{Cardiff}}{one of the four methods of preparation of the steel plates}
  }
}
\references{
Wu, C.F.J. and M. Hamada (2000-9). Experiments: Planning, Analysis, and Parameter
Design Optimization, 2e. J. Wiley.
}
\examples{
data(girder)
girder
boxplot(girder[,2:5])
}
\keyword{median polish}
