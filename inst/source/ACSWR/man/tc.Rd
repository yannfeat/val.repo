\name{tc}
\alias{tc}
\docType{data}
\title{
The Toluca Company Labour Hours against Lot Size
}
\description{
The Toluca Company manufactures equipment related to refrigerator. The company, in respect of a particular component of a refrigerator, has data on the labor hours required for the component in various lot sizes. Using this data, the officials wanted to find the optimum lot size for producing this part.
}
\usage{data("tc")}
\format{
  A data frame with 25 observations on the following 2 variables.
  \describe{
    \item{\code{Lot_Size}}{size of the lot}
    \item{\code{Labour_Hours}}{the labor hours required}
  }
}
\references{
Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (1974-2005). Applied
Linear Statistical Models, 5e. McGraw-Hill.
}
\examples{
data(tc)
tclm <- lm(Labour_Hours~Lot_Size,data=tc)
tclm$coefficients
}
\keyword{linear regression model}
