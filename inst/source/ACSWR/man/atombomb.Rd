\name{atombomb}
\alias{atombomb}
\docType{data}
\title{
Japanese atomic bomb survivors
}
\description{
Gore, et al. (2006) consider the frequencies of cancer deaths of Japanese atomic bomb survivors by extent of exposure, years after exposure, etc. This data set has appeared in the journal "Statistical Sleuth".
}
\usage{data("atombomb")}
\format{
  A data frame with 84 observations on the following 4 variables.
  \describe{
    \item{\code{Radians}}{Extent of exposure to the radian levels}
    \item{\code{Count.Type}}{the type of count \code{At Risk} \code{Death Count}}
    \item{\code{Count.Age.Group}}{age group with levels \code{'0-7'} \code{'12-15'} \code{'16-19'} \code{'20-23'} \code{'24-27'} \code{'28-41'} \code{'8-11'}}
    \item{\code{Frequency}}{the count of deaths}
  }
}
\references{
Gore, A.P., Paranjape, S. A., and Kulkarni, M.B. (2006). 100 Data Sets for Statistics Education. Department of Statistics, University of Pune.}
\examples{
data(atombomb)
atombombxtabs <- xtabs(Frequency~Radians+Count.Type+Count.Age.Group,data=atombomb)
atombombxtabs
}
\keyword{categorical data analysis}