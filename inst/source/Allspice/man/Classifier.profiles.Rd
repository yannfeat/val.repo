\name{profiles<-,Classifier-method}
\alias{profiles<-,Classifier-method}
\alias{profiles<-}
\alias{profiles}

\title{
Sample data profiles
}

\description{
Analyse new data using classification assets.
}

\usage{
profiles(obj) <- value
}

\arguments{
  \item{obj}{
An object of the class Classifier.
  }
  \item{value}{
A numeric vector or a matrix where samples are organized into
columns and variables into rows.
  }
}

\details{
If the input is a vector, the elements must be named and these
names will be used to identify variables.

If the input is a matrix, it must have sample identities as column
names and variables identified by row names.
}

\value{
   Updates the Classifier object. Any previous data are discarded.
}

\examples{
# Simulated data.
simu <- bcellALL(5)

# Predict subtypes.
cls <- classifier(verbose = FALSE)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
primary <- predictions(cls)[[1]]
print(primary[,c("LABEL","PROX","EXCL")])
}