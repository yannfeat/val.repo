\name{information,Classifier-method}
\alias{information,Classifier-method}
\alias{information}

\title{
Classifier contents
}

\description{
Information about the assets of a classifier
}

\usage{
information(obj)
}

\arguments{
  \item{obj}{
An object of the class Classifier.
  }
}

\value{
A list with three elements: \code{covariates} is a data frame that contains
the variable names that are included in the classification assets,
\code{configuration} contains the analysis settings for each asset and
\code{categories} contains the labels and visual attributes for the assets.
}

\examples{
# Show the contents of the b-cell ALL classifier.
cls <- classifier(verbose=FALSE)
info <- information(cls)
print(info$covariates)
print(info$configuration)
print(head(info$categories))
print(tail(info$categories))
}