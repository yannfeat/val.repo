\name{usc}
\alias{usc}
\docType{data}
\title{
US Crime Data
}
\description{
Data is available on the crime rates across 47 states in USA, and we have additional information on 13 more explanatory variables.
}
\usage{data(usc)}
\format{
  A data frame with 47 observations on the following 14 variables.
  \describe{
    \item{\code{R}}{Crime rate - the number of offenses known to the police per 1,000,000 population}
    \item{\code{Age}}{Age distribution - the number of males aged 14 to 24 years per 1000 of total state population}
    \item{\code{S}}{Binary variable distinguishing southern states (S = 1) from the rest}
    \item{\code{Ed}}{Educational level - mean number of years of schooling times 10 of the population 25 years old and over}
    \item{\code{Ex0}}{Police expenditure - per capita expenditure on police protection by state and local governments in 1960}
    \item{\code{Ex1}}{Police expenditure - as Ex0, but for 1959}
    \item{\code{LF}}{Labour force participation rate per 1000 civilian urban males in the age group 14 to 24 years}
    \item{\code{M}}{Number of males per 1000 females}
    \item{\code{N}}{State population size in hundred thousands}
    \item{\code{NW}}{Number of non-whites per 1000}
    \item{\code{U1}}{Unemployment rate of urban males per 1000 in the age group 14 to 24 years}
    \item{\code{U2}}{Unemployment rate of urban males per 1000 in the age group 35 to 39 years}
    \item{\code{W}}{Wealth, as measured by the median value of transferable goods and assets. or family income (unit 10 dollars)}
    \item{\code{X}}{Income inequality: the number of families per 1000 earning below one half of the median income}
  }
}
\references{
Der, G., and Everitt, B.S. (2002). A Handbook of Statistical Analysis using SAS, 2e. CRC.}
\examples{
data(usc)
pairs(usc)
round(cor(usc),2)
}
\keyword{multiple linear regression model}