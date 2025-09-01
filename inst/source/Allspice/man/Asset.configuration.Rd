\name{configuration<-,Asset-method}
\alias{configuration<-,Asset-method}
\alias{configuration,Asset-method}
\alias{configuration<-}
\alias{configuration}

\title{
Asset configuration
}

\description{
Set internal parameters for an Asset object.
}

\usage{
configuration(obj) <- value
configuration(obj)
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{value}{
A numeric vector with named elements.
  }
}

\details{
Element names from the input are compared with the internal list
of parameters. Those that match will be updated.

Normalization parameters include 'norm' (if set to 0, normalization
is not performed), 'nonzero.min' (the minimum data value considered larger
than zero) and 'nonzero.ratio' (minimum ratio of non-zero values to include
a variable in the output). See \code{\link{normalize}()} for additional
details.

Standardization parameters include 'standard' (if set to 0, standardization
is not performed) and 'logarithm' (if set to 0, data values are used without
taking the logarithm). See \code{\link{standardize}()} for additional
details.

Feature selection parameters include 'ninput.max' (the maximum number of
features to be used for classification) and 'rrinput.max' (the maximum
correlation r-squared to be allowed between features). See
\code{\link{assemble}()} for details on selecting non-redundant inputs.

Classification parameters include 'probability.min' (minimum probability
rating considered for reliable classification) and 'exclusivity.min'
(minimum exclusivity for non-ambiguous classification).
See \code{\link{classify}()} for additional details.
}

\value{
Updates the Asset object.
}

\examples{
# Change asset configuration.
a <- asset()
print(configuration(a))
configuration(a) <- c(nonzero.min=0, nonzero.ratio=0)
print(configuration(a))
}