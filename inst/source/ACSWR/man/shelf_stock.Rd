\name{shelf_stock}
\alias{shelf_stock}
\docType{data}
\title{
The Shelf-Stocking Data
}
\description{
A merchandiser stocks soft-drink on a shelf as a multiple number of the number of cases. The time required to put the cases in the shelves is recorded as a response. Clearly, if there are no cases to be stocked, it is natural that the time to put them on the shelf will be 0.
}
\usage{data("shelf_stock")}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{Time}}{time required to put the cases in the shelves}
    \item{\code{Cases_Stocked}}{number of cases}
  }
}

\examples{
data(shelf_stock)
sslm <- lm(Time ~ Cases_Stocked -1, data=shelf_stock)
}
\keyword{linear regression model}
