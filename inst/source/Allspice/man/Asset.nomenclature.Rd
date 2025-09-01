\name{nomenclature<-,Asset-method}
\alias{nomenclature<-,Asset-method}
\alias{nomenclature,Asset-method}
\alias{nomenclature<-}
\alias{nomenclature}

\title{
Variable names
}

\description{
Conversion table between variable naming schemes.
}

\usage{
nomenclature(obj) <- value
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{value}{
A data frame, see details.
  }
}

\details{
    The data frame should contain character columns of variable names
    with each column representing a spefic naming scheme from which
    the variables are translated into the row names of the data frame.

    For example, to convert gene symbols into Ensemble codes, a data frame
    with the gene symbols as row names and one column of ensemble codes
    is needed.
}

\value{
   Updates the Asset object.
}

\examples{
# Import nomenclature from a system file.
base <- system.file(package = "Allspice")
fname <- file.path(base, "subtypes", "nomenclature.txt")
info <- read.delim(fname, stringsAsFactors = FALSE)

# Set ENSEMBLE identities as row names.
rownames(info) <- info$ENSEMBL
info$ENSEMBL <- NULL
print(head(info))

# Create a new asset and set nomenclature.
a <- asset()
nomenclature(a) <- info

# Prepare training data.
simu <- bcellALL(200)
materials <- list(title="Simutypes")
materials$dat <- simu$counts
materials$covariates <- simu$metadata[,c("MALE","AGE")]
materials$bits <- simu$metadata[,"SUBTYPE",drop=FALSE]

# Assemble classification asset.
assemble(a) <- materials

# Check that nomenclature was set.
simu <- bcellALL(5)
expres <- normalize(a, dat = simu$counts)
print(head(simu$counts))
print(head(expres))
}