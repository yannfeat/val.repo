\name{export,Asset-method}
\alias{export,Asset-method}
\alias{export}

\title{
Data classification
}

\description{
Assigns category labels to new data.
}

\usage{
export(obj, folder)
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{folder}{
Path to a folder that will contain the asset files.
  }
}

\value{
   Returns the names of the files that were saved in the folder.
}

\examples{
# Import ALL subtyping asset.
base <- system.file(package = "Allspice")
folder <- file.path(base, "subtypes")
a <- asset(folder)

# Export asset into a new folder.
tpath <- tempfile()
fnames <- export(a, folder = tpath)
print(dir(tpath))
}