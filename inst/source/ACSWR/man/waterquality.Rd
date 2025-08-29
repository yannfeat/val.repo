\name{waterquality}
\alias{waterquality}
\docType{data}
\title{
Testing for Physico-chemical Properties of Water in 4 Cities
}
\description{
Water
samples from four cities are collected and their physico-chemical properties for ten variables, such as \code{pH}, \code{Conductivity}, \code{Dissolution}, etc., are measured. We would then like to test if the properties are same across the four cities and in which case a same water treatment approach can be adopted across the cities.
}
\usage{data(waterquality)}
\format{
  A data frame with 63 observations on the following 10 variables.
  \describe{
    \item{\code{City}}{four cities \code{City1} \code{City2} \code{City3} \code{City4}}
    \item{\code{pH}}{the pH concentration}
    \item{\code{Conductivity}}{water conductivity}
    \item{\code{Dissolution}}{water dissolution}
    \item{\code{Alkalinity}}{alkalinity of the water sample}
    \item{\code{Hardness}}{water hardness}
    \item{\code{Calcium.Hardness}}{calcium hardness of the water}
    \item{\code{Magnesium.Hardness}}{magnesium hardness of the water}
    \item{\code{Chlorides}}{chloride content}
    \item{\code{Sulphates}}{sulphate content}
  }
}
\references{
Gore, A.P., Paranjape, S. A., and Kulkarni, M.B. (2006). 100 Data Sets for Statistics Education. Department of Statistics, University of Pune.
}
\examples{
data(waterquality)
}
\keyword{Manova}