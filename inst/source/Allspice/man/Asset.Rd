\name{asset}
\alias{asset}
\alias{Asset-class}

\title{
Constructor for Asset
}

\description{
Creates a new Asset object.
}

\usage{
asset(folder = NULL, verbose = TRUE)
}

\arguments{
  \item{folder}{
Path to a folder that contains the necessary
files for a classification asset.
  }
  \item{verbose}{
If TRUE, accessed asset items (file names) are printed on screen.
}
}

\value{
Returns an Asset object.
}
\examples{
# Set up an ALL subtyping asset.
folder <- system.file("subtypes", package="Allspice")
a <- asset(folder)
}