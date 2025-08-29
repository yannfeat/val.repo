\name{reaction}
\alias{reaction}
\docType{data}
\title{
Chemical Reaction Experiment
}
\description{
For a chemical reaction experiment, the blocks arise due to the Batch number, Catalyst of different types form the treatments, and the reaction time is the output. Due to a restriction, all the catalysts cannot be analysed within each batch and hence we need to look at the BIBD model.
}
\usage{data("reaction")}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{Catalyst}}{different types forming the treatments}
    \item{\code{Batch}}{batch number}
    \item{\code{Reaction}}{reaction time}
  }
}
\examples{
data(reaction)
}
\keyword{Balanced incomplete block design}
