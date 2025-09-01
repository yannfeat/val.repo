\name{visuals<-,Asset-method}
\alias{visuals<-,Asset-method}
\alias{visuals,Asset-method}
\alias{visuals<-}
\alias{visuals}

\title{
Visual attributes
}

\description{
Attach text and color attributes to categories.
}

\usage{
visuals(obj) <- value
}

\arguments{
  \item{obj}{
An object of the class Asset.
  }
  \item{value}{
A character vector or a data frame, see details.
  }
}

\details{
   If the input value is a character vector, the elements are stored as the
   category text and names of the elements are stored as category names.

   If the input value is a data frame, the column 'LABEL' is used as the text
   and the row names as the names of the categories. Additional columns may
   include 'COLOR', 'COLOR.dark' and 'COLOR.light' that must contain strings
   of color names or hexadecimal codes as produced by \code{\link{rgb}()}.
   In absence of color data, the function assigns automatic colors.
}

\value{
   Updates the Asset object.
}

\examples{
# Create a new asset and set nomenclature.
a <- asset()

# Set category labels with automatic colors.
labels <- paste("Category", 1:8)
names(labels) <- paste0("cat", 1:8)
visuals(a) <- labels
print(a@categories)

# Add color information.
info <- data.frame(stringsAsFactors = FALSE,
    LABEL = labels, COLOR = "red")
rownames(info) <- names(labels)
visuals(a) <- info
print(a@categories)
}