\name{intensity}
\alias{intensity}
\docType{data}
\title{
Blocking for Intensity Data Set
}
\description{
The intent of this experiment is to help the engineer in improving the ability of detecting targets on a radar system. The two variables chosen which are believed to have the most impact on the detecting abilities of the radar system are marked as the amount of the background noise and the type of filter on the screen.
}
\usage{data(intensity)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{Intensity}}{intensity of targets}
    \item{\code{Operator}}{different operators who form the blocks \code{1} \code{2} \code{3} \code{4}}
    \item{\code{Filter}}{two types of filter \code{1} \code{2}}
    \item{\code{Ground}}{the type of background noise \code{high} \code{low} \code{medium}}
  }
}
\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(intensity)
intensity.aov <- aov(Intensity~Ground*Filter+Error(Operator),intensity)
summary(intensity.aov)
intensity.aov
}
\keyword{blocked design}
