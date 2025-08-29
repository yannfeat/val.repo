\name{flight}
\alias{flight}
\docType{data}
\title{
Injuries in Airflights
}
\description{
Injuries in airflights, road accidents, etc, are instances of rare occurrences which are appropriately modeled by a Poisson distribution. Two models, before and after transformation, are fit and it is checked if the transformation led to a reduction to the variance.
}
\usage{data(flight)}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{Injury_Incidents}}{Count of injury incidents}
    \item{\code{Total_Flights}}{Total number of flights}
  }
}
\references{
Chatterjee, S., and Hadi, A. S. (1977-2006). Regression Analysis by Examples, 4e.
J. Wiley.
}
\examples{
data(flight)
names(flight)
injurylm <- lm(Injury_Incidents~Total_Flights,data=flight)
injurysqrtlm <- lm(sqrt(Injury_Incidents)~Total_Flights,data=flight)
summary(injurylm)
summary(injurysqrtlm)
}
\keyword{transformation, variance reduction}
