\name{cloud}
\alias{cloud}
\docType{data}
\title{
The Cloud Seeding Data
}
\description{
Chambers, et al. (1983), page 381, contains the cloud seeding data set. Rainfall in acre-feet for 52 clouds are measured, 50\% of which have natural rain (control group) whereas the others are seeded. We need to visually compare whether seeding the clouds lead to increase in rainfall in acre-feet.
}
\usage{data(cloud)}
\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{Control}}{Rainfall in acre-feet for 26 clouds are measured which had natural rain, that is, control group}
    \item{\code{Seeded}}{Rainfall in acre-feet for 26 clouds are measured which had seeded rain}
  }
}
\references{
Chambers, J.M., Cleveland, W.S., Kleiner, B., and Tukey, P.A. (1983). Graphical
Methods for Data Analysis. Wadsworth and Brooks/Cole.
}
\examples{
data(cloud)
stem(log(cloud$Seeded),scale=1)
stem(log(cloud$Control),scale=1)
}
\keyword{stem-and-leaf plot, cloud seeding experiment}
