\name{classifier}
\alias{classifier}
\alias{Classifier-class}

\title{
Constructor for Classifier
}

\description{
Creates a new Classifier object.
}

\usage{
classifier(..., verbose = TRUE)
}

\arguments{
  \item{...}{
      Any number of paths to folders that contain assets
      (see \code{\link{asset}()}).
  }
  \item{verbose}{
      If TRUE, information about the imported assets is printed on screen.
  }  
}

\details{
The first input argument will set the primary asset, and the others
will be considered secondary assets.
}

\value{
Returns a Classifier object.
}
\examples{
# Set up an ALL classifier object.
cls <- classifier()
}
