\name{predictions,Classifier-method}
\alias{predictions,Classifier-method}
\alias{predictions}

\title{
Category predictions
}

\description{
Classifies samples based on their profiles.
}

\usage{
predictions(obj)
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
classification asset within the classifier. See \code{\link{classify}()}
for details on the result items.
}

\examples{
# Simulated data.
simu <- bcellALL(5)

# Predict subtypes.
cls <- classifier(verbose = FALSE)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
pred <- predictions(cls)
print(pred[[1]][,c("LABEL","PROX","EXCL")])
print(pred[[2]][,c("LABEL","PROX","EXCL")])
print(pred[[3]][,c("LABEL","PROX","EXCL")])
}