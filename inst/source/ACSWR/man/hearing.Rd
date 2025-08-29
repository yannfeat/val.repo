\name{hearing}
\alias{hearing}
\docType{data}
\title{
Hearing Loss Data
}
\description{
A study was carried in the Eastman Kodak Company which involved the measurement of hearing loss. Such studies are called as audiometric study. This data set contains 100 males, each aged 39, who had no indication of noise exposure or hearing disorders. Here, the individual is exposed to a signal of a given frequency with an increasing intensity till the signal is perceived.
}
\usage{data(hearing)}
\format{
  A data frame with 100 observations on the following 9 variables.
  \describe{
    \item{\code{Sl_No}}{Serial Number}
    \item{\code{L500}}{Observation for 500Hz in the left ear}
    \item{\code{L1000}}{Observation for 1000Hz in the left ear}
    \item{\code{L2000}}{Observation for 2000Hz in the left ear}
    \item{\code{L4000}}{Observation for 4000Hz in the left ear}
    \item{\code{R500}}{Observation for 500Hz in the right ear}
    \item{\code{R1000}}{Observation for 1000Hz in the right ear}
    \item{\code{R2000}}{Observation for 2000Hz in the right ear}
    \item{\code{R4000}}{Observation for 4000Hz in the right ear}
  }
}
\references{
Jackson, J.E. (1991). A User's Guide to Principal Components. New York: Wiley.
}
\examples{
data(hearing)
round(cor(hearing[,-1]),2)
round(cov(hearing[,-1]),2)
hearing.pc <- princomp(hearing[,-1])
screeplot(hearing.pc,main="B: Scree Plot for Hearing Loss Data")
}
\keyword{principal component analysis}
