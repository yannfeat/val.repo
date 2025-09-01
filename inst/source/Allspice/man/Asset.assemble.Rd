\name{assemble<-,Asset-method}
\alias{assemble<-,Asset-method}
\alias{assemble,Asset-method}
\alias{assemble<-}
\alias{assemble}

\title{
Finish Asset contents
}

\description{
Trains a new classification model.
}

\usage{
assemble(obj) <- value
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{value}{
A list that containts training data, see details.
  }
}

\details{
The value argument must contain three named elements: \code{title},
\code{dat} and \code{bits}. Optional \code{predictors} and
\code{covariates} elements can also be included.

The title is a descriptive identifier for the asset that will be
displayed by the Classifier object in \code{\link{report}()}.

The \code{dat} element is a matrix that contains the training samples.
The variables are organized into named rows and the samples into
named columns. Non-finite values are not allowed.

The \code{predictors} element contains the names of the input variables
that should be used for training the model. If empty, all inputs are
used for automatic feature selection and subsequent training steps.

The \code{covariates} element contains additional information for constructing
the final classification models. Unlike the data matrix, variables
are organized into named columns and the samples are stored as rows.
Non-finite values are not allowed.

The \code{bits} element contains labels for category memberships. Three
formats are supported: 1) a character vector of named elements that
contains non-empty strings, 2) a matrix or a data frame with row names
and a single column of non-empty values, and 3) a matrix or a data frame
with multiple columns that contain binary values where 1s indicate
category membership (the name of the column is the name of the
category). Overlapping categories are allowed.

The final asset is assembled in six steps. First, the training data are
standardized and normalized. Second, input variables are sorted according
to their univariate classification performance. Third, redundant features
are excluded by testing the sorted variables for mutual correlations; this
produces an optimized listing of non-redundant features that are the most
predictive of the category labels. Fourth, mean centroids are calculated for
each category. Fifth, training samples are matched to their nearest
centroids and the distances collected as preliminary predictor scores.
Lastly, logistic regression models are fitted to the preliminary scores,
covariates and category labels to enable the calculation of standardized
predictor scores for new data.
}

\value{
Updates the Asset object.
}

\examples{
# Prepare training data.
simu <- bcellALL(200)
materials <- list(title="Simutypes")
materials$dat <- simu$counts
materials$covariates <- simu$metadata[,c("MALE","AGE")]
materials$bits <- simu$metadata[,"SUBTYPE",drop=FALSE]

# Assemble classification asset.
bALL <- asset()
assemble(bALL) <- materials

# Export asset into a new folder.
tpath <- tempfile()
export(bALL, folder = tpath)

# Create a classifier.
cls <- classifier(tpath, verbose = FALSE)

# Classify new samples.
simu <- bcellALL(5)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
primary <- predictions(cls)[[1]]
print(primary[,c("LABEL","PROX","EXCL")])
}