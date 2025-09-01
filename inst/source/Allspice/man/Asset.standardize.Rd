\name{standardize,Asset-method}
\alias{standardize,Asset-method}
\alias{standardize}

\title{
Data standardization
}

\description{
Standardize scale and location of variables.
}

\usage{
standardize(obj, dat, trim = FALSE)
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{dat}{
A matrix that containts variables as rows and samples as columns.
  }
  \item{trim}{
If true, returns only variables used as input features for classification.
  }
}

\details{
If the asset is so configured, the data are first transformed by log(x + 1).
Values are processed with the mean and standard deviation that were
calculated from the training data when the asset was assembled.
The mean is subtracted and the values divided by SD. To control for
outliers, extreme values are compressed by the t-distribution with 50
degrees of freedom.
}

\value{
Returns a matrix in the same format as the input.
}

\examples{
# Import ALL subtyping asset.
base <- system.file(package = "Allspice")
folder <- file.path(base, "subtypes")
a <- asset(folder)

# Simulated data.
simu <- bcellALL(5)

# Standardize RNA read counts.
expres <- normalize(a, dat = simu$counts)
zscores <- standardize(a, dat = expres)
print(head(simu$counts))
print(head(expres))
print(head(zscores))
}