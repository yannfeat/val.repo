\name{olson}
\alias{olson}
\docType{data}
\title{
The Olson Heart Lung Dataset
}
\description{
We need to determine the effect of the number of revolutions per minute (rpm) of the rotary pump head of an Olson heart-lung pump on the fluid flow rate \code{Liters_minute}. The rpm's are replicated at 50, 75, 100, 125, and 150 levels with respective frequencies 5, 3, 5, 2, and 5. The fluid flow rate is measured in litters per minute.
}
\usage{data(olson)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{Observation}}{observation number}
    \item{\code{rpm}}{rmp levels at 50, 75, 100, 125, and 150}
    \item{\code{Level}}{the rpm levels}
    \item{\code{Liters_minute}}{litters per minute}
  }
}
\references{
Dean, A., and Voss, D. (1999). Design and Analysis of Experiments. Springer.
}
\examples{
data(olson)
par(mfrow=c(2,2))
plot(olson$rpm,olson$Liters_minute,xlim=c(25,175),xlab="RPM",
  ylab="Flow Rate",main="Scatter Plot")
boxplot(Liters_minute~rpm,data=olson,main="Box Plots")
aggregate(olson$Liters_minute,by=list(olson$rpm),mean)
olson_crd <- aov(Liters_minute ~ as.factor(rpm), data=olson)
}
\keyword{completely randomized design}
