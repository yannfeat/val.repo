\name{caesareans}
\alias{caesareans}
\docType{data}
\title{
The Cesarean Cases
}
\description{
An increasing concern has been the number of cesarean deliveries, especially in the private hospitals. Here, we know the number of births, the type of hospital (private or Government hospital), and the number of cesareans. We would like to model the number of cesareans as a function of the number of births and the type of hospital. A Poisson regression model is fitted for this data set.}
\usage{data(caesareans)}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Births}}{total number of births}
    \item{\code{Hospital_Type}}{type of hospital, private or government}
    \item{\code{Caesareans}}{number of caesareans}
  }
}
\source{
http://www.oxfordjournals.org/our_journals/tropej/online/ma_chap13.pdf
}
\examples{
data(caesareans)
names(caesareans)
cae_pois <- glm(Caesareans~Hospital_Type+Births,data=caesareans,family='poisson')
summary(cae_pois)
}
\keyword{Poisson regression model}
