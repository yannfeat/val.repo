\name{chemicaldata}
\alias{chemicaldata}
\docType{data}
\title{
Chemical Reaction Experiment
}
\description{
This data set is used to illustrate the concept of canonical correlations. Here, temperature, concentration, and time have influence on three yield variables, namely outputs, while the percentage of unchanged starting material, the percentage converted to the desired product, and the percentage of unwanted by-product form another set of related variables. 
}

\usage{data(chemicaldata)}
\format{
  A data frame with 19 observations on the following 6 variables.
  \describe{
    \item{\code{y1}}{the percentage of unchanged starting material}
    \item{\code{y2}}{the percentage converted to the desired product}
    \item{\code{y3}}{the percentage of unwanted by-product}
    \item{\code{x1}}{temperature}
    \item{\code{x2}}{concentration}
    \item{\code{x3}}{time}
  }
}
\source{
Box, G. E. P., and Youle, P. V. (1955). The Exploration of Response Surfaces: An
Example of the Link between the Fitted Surface and the Basic Mechanism of
the System. Biometrics, 11, 287-323.
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.
}
\examples{
data(chemicaldata)
names(chemicaldata)
chemicaldata$x12 <- chemicaldata$x1*chemicaldata$x2; 
chemicaldata$x13 <- chemicaldata$x1*chemicaldata$x3; 
chemicaldata$x23 <- chemicaldata$x2*chemicaldata$x3
chemicaldata$x1sq <- chemicaldata$x1^{2}
chemicaldata$x2sq <- chemicaldata$x2^{2}
chemicaldata$x3sq <- chemicaldata$x3^{2}
S_Total <- cov(chemicaldata)
cancor_xy <- sqrt(eigen(solve(S_Total[1:3,1:3])\%*\%S_Total[1:3,
4:12]\%*\%solve(S_Total[4:12,4:12])\%*\%S_Total[4:12,1:3])$values)
cancor_xy
cancor(chemicaldata[,1:3],chemicaldata[,4:12])
}
\keyword{canonical correlation}
