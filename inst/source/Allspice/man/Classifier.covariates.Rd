\name{covariates<-,Classifier-method}
\alias{covariates<-,Classifier-method}
\alias{covariates,Classifier-method}
\alias{covariates<-}
\alias{covariates}

\title{
Sample covariate data
}

\description{
Add covariate data into the classifier.
}

\usage{
covariates(obj) <- value
}

\arguments{
  \item{obj}{
An object of the class Classifier.
  }
  \item{value}{
A numeric vector or a matrix.
  }
}

\details{
If the input is a vector, the elements must be named and these
names will be used to identify variables.

If the input is a matrix, it must have named rows and named columns that
will be matched with sample identities in \code{\link{profiles}()}.
}

\value{
Updates the Classifier object. Any previous data are discarded.
}

\examples{
# Simulated data.
simu <- bcellALL(5)

# Predict subtypes without covariates.
cls <- classifier(verbose = FALSE)
profiles(cls) <- simu$counts
primary <- predictions(cls)[[1]]
print(primary[,c("LABEL","PROX","EXCL")])

# Predict subtypes with covariates.
cls <- classifier(verbose = FALSE)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
primary <- predictions(cls)[[1]]
print(primary[,c("LABEL","PROX","EXCL")])
}