\name{normalize,Asset-method}
\alias{normalize,Asset-method}
\alias{normalize}

\title{
Sample normalization
}

\description{
Adjust scale differences between samples.
}

\usage{
normalize(obj, dat)
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{dat}{
A matrix that containts variables as rows and samples as columns.
  }
}

\details{
The normalization pipeline comprises three steps. First, variable names
are checked against the internal nomenclature and converted to the
internal naming scheme where necessary (see \code{\link{nomenclature}()}).
Second, variables that are present in the internal normalization reference
are imputed with reference values if not available in the data. Third, the
data are normalized according to the DESeq2 algortihm (Love MI, Huber W &
Anders S, Moderated estimation of fold change and dispersion for RNA-seq
data with DESeq2, Genome Biol 15, 550, 2014).
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

# Normalize RNA read counts.
expres <- normalize(a, dat = simu$counts)
print(head(simu$counts))
print(head(expres))
}