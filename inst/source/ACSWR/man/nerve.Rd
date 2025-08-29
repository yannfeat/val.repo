\name{nerve}
\alias{nerve}
\docType{data}
\title{
The Nerve Data
}
\description{
The Nerve data set has been popularized by Cox and Lewis (1966). In this experiment 799 waiting times are recorded for successive pulses along a nerve fiber.
}
\usage{data(nerve)}
\format{
  The format is:
 num [1:799] 0.21 0.03 0.05 0.11 0.59 0.06 0.18 0.55 0.37 0.09 ...
}
\source{
Cox, D. and Lewis, P. (1966). The Statistical Analysis of Series of Events. Chapman & Hall.
}
\examples{
data(nerve)
nerve_ecdf <- ecdf(nerve)
knots(nerve_ecdf) # Returns the jump points of the edf
summary(nerve_ecdf) # the usual R summaries
nerve_ecdf(nerve) # returns the percentiles at the data points
}
\keyword{empirical distribution function}
