\name{insurance}
\alias{insurance}
\docType{data}
\title{
Insurance Claims Data
}
\description{
Montgomery (2005), page 42, describes this data set in which the number of days taken by the company to process and settle the claims of employee health insurance customers. The data is recorded for the number of days for settlement from the first to fortieth claim.
}
\usage{data(insurance)}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{Claim}}{Claim number}
    \item{\code{Days}}{Days to settle the claim amount}
  }
}
\references{
Montgomery, D.C. (1985-2012). Introduction to Statistical Quality Control, 7e. J. Wiley.
}
\examples{
data(insurance)
plot(insurance$Claim,insurance$Days,"l",xlab="Claim Sequence",
   ylab="Time to Settle the Claim")
title("B: Run Chart for Insurance Claim Settlement")
}
\keyword{time series plot}
