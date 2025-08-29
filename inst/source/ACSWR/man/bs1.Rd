\name{bs1}
\alias{bs1}
\docType{data}
\title{
British Doctors Smoking and Coronary Heart Disease
}
\description{
The problem is to investigate the impact of smoking tobacco among British doctors, refer Example 9.2.1 of Dobson. In the year 1951, a survey was sent across among all the British doctors asking them whether they smoked tobacco and their age group \code{Age_Group}. The data also collects the person-years \code{Person_Years} of the doctors in the respective age group. A follow-up after ten years reveals the number of deaths Deaths, the smoking group indicator \code{Smoker_Cat}.
}
\usage{data(bs1)}
\format{
  A data frame with 10 observations on the following 9 variables.
  \describe{
    \item{\code{Age_Group}}{a factor variable of age group with levels \code{35-44} \code{45-54} \code{55-64} \code{65-74} \code{75-84}}
    \item{\code{Age_Cat}}{slightly re-coded to extract variables with \code{Age_Cat} taking values 1-5 respectively for the age groups 35-44, 45-54, 55-64, 65-74, and 75-84}
    \item{\code{Age_Square}}{square of the variable \code{Age_Cat}}
    \item{\code{Smoker_Cat}}{the smoking group indicator \code{NO} \code{YES}}
    \item{\code{Smoke_Ind}}{a numeric vector}
    \item{\code{Smoke_Age}}{takes the Age_Cat values for the smokers group and 0 for the non-smokers}
    \item{\code{Deaths}}{a follow-up after ten years revealing the number of deaths}
    \item{\code{Person_Years}}{the number of deaths standardized to 100000}
    \item{\code{Deaths_Per_Lakh_Years}}{a numeric vector}
  }
}
\source{
Dobson (2002)
}
\references{
Dobson, A. J. (1990-2002). An Introduction to Generalized Linear Models, 2e.
Chapman & Hall/CRC.
}
\examples{
library(MASS)
data(bs1)
BS_Pois <-  glm(Deaths~Age_Cat+Age_Square+Smoke_Ind+Smoke_Age,offset=
log(Person_Years),data=bs1,family='poisson')
logLik(BS_Pois)
summary(BS_Pois)
with(BS_Pois, pchisq(null.deviance - deviance,df.null - 
df.residual,lower.tail = FALSE)) 
confint(BS_Pois)}
\keyword{Poisson regression}
