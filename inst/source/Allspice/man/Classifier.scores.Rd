\name{scores,Classifier-method}
\alias{scores,Classifier-method}
\alias{scores}

\title{
Category scores
}

\description{
Classification scores for samples based on their profiles.
}

\usage{
scores(obj)
}

\arguments{
  \item{obj}{
An object of the class Classifier.
  }
}

\details{
Use the functions \code{\link{covariates}()} and
\code{\link{profiles}()} to import data into the classifier.
}

\value{
Returns a list of data frames that contain the output from each
classification asset within the classifier.
}

\examples{
# Simulated data.
simu <- bcellALL(5)

# Predict subtypes.
cls <- classifier(verbose = FALSE)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
z <- scores(cls)
print(z[[1]][,1:5])
print(z[[2]][,1:5])
print(z[[3]][,1:5])
}