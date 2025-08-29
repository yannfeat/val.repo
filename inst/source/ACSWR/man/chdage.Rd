\name{chdage}
\alias{chdage}
\docType{data}
\title{
Coronary Heart Disease}
\description{
A well known explanation of the heart disease is that as the age increases, the risk of coronary heart disease also increase. The current data set and the example may be found in Chapter 1 of Hosmer and Lemeshow (1990-2013).
}
\usage{data(chdage)}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{ID}}{patient ID}
    \item{\code{AGE}}{age of the patient}
    \item{\code{CHD}}{Coronary Heart Disease indicator}
  }
}
\source{
Hosmer and Lemeshow (1990-2013).
}
\references{
Hosmer, D.W., and Lemeshow, S. (1990-20013). Applied Logistic Regression, 3e.
Wiley.
}
\examples{
data(chdage)
plot(chdage$AGE,chdage$CHD,xlab="AGE",ylab="CHD Indicator", 
main="Scatter plot for CHD Data")
agegrp <- cut(chdage$AGE,c(19,29,34,39,44,49,54,59,69),include.lowest=TRUE,
labels=c(25,seq(31.5,56.5,5),64.5))
mp <- c(25,seq(31.5,56.5,5),64.5) # mid-points
chd_percent <- prop.table(table(agegrp,chdage$CHD),1)[,2]
points(mp,chd_percent,"l",col="red")
}
\keyword{logistic regression}
