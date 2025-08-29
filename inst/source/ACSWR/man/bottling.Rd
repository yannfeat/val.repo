\name{bottling}
\alias{bottling}
\docType{data}
\title{
A Three Factorial Experiment for Bottling Data}
\description{
The height of the fills in the soft drink bottle is required to be as consistent as possible and it is controlled through three factors: (i) the percent carbonation of the drink, (ii) the operating pressure in the filler, and (iii) the line speed which is the number of bottles filled per minute. The first factor variable of the percent of carbonation is available at three levels of 10, 12, and 14, the operating pressure is at 25 and 30 psi units, while the line speed are at 200 and 250 bottles per minute. Two complete replicates are available for each combination of the three factor levels, that is, 24 total number of observations. In this experiment, the deviation from the required height level is measured.}
\usage{data(bottling)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{Deviation}}{deviation from required height level}
    \item{\code{Carbonation}}{the percent carbonation of the drink}
    \item{\code{Pressure}}{the operating pressure in the filler}
    \item{\code{Speed}}{the number of bottles filled per minute}
  }
}
\source{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(bottling)
summary(bottling.aov <- aov(Deviation~.^3,bottling))
# Equivalent way
summary(aov(Deviation~ Carbonation + Pressure + Speed+ (Carbonation*Pressure)+
(Carbonation*Speed)+(Pressure*Speed)+(Carbonation*Speed*Pressure),data=bottling)) 
}
\keyword{three factorial experiment}
