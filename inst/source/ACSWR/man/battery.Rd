\name{battery}
\alias{battery}
\docType{data}
\title{
Two Factorial Experiment for Battery Data
}
\description{
An experiment where the life of a battery is modeled as a function of the extreme variations in temperature of three levels 15, 70, and 1250 Fahrenheit and three type of plate material. Here, the engineer has no control on the temperature variations once the device leaves the factory. Thus, the task of the engineer is to investigate two major problems: (i) The effect of material type and temperature on the life of the device, and (ii) Finding the type of material which has least variation among the varying temperature levels. For each combination of the temperature
and material, 4 replications of the life of battery are tested.}
\usage{data(battery)}
\format{
  A data frame with 36 observations on the following 3 variables.
  \describe{
    \item{\code{Life}}{battery life}
    \item{\code{Material}}{the type of plate material}
    \item{\code{Temperature}}{three extreme variations of temperature}
  }
}
\source{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(battery)
names(battery) <- c("L","M","T")
battery$M <- as.factor(battery$M)
battery$T <- as.factor(battery$T)
battery.aov <- aov(L~M*T,data=battery)
model.matrix(battery.aov)
summary(battery.aov)
}
\keyword{factorial experiment}
