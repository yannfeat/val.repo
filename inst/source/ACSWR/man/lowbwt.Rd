\name{lowbwt}
\alias{lowbwt}
\docType{data}
\title{
The Low-Birth Weight Problem
}
\description{
Low birth weight of new-born infants is a serious concern. If the weight of the new-born is less than 2500 grams, we consider that instance as a low-birth weight case. A study was carried out at Baystate Medical Center in Springfield, Massachusetts.
}
\usage{data(lowbwt)}
\format{
  A data frame with 189 observations on the following 10 variables.
  \describe{
    \item{\code{LOW}}{Low Birth Weight}
    \item{\code{AGE}}{Age of Mother}
    \item{\code{LWT}}{Weight of Mother at Last Menstrual Period}
    \item{\code{RACE}}{Race \code{1} \code{2} \code{3}}
    \item{\code{SMOKE}}{Smoking Status During Pregnancy}
    \item{\code{PTL}}{History of Premature Labor}
    \item{\code{HT}}{History of Hypertension}
    \item{\code{UI}}{Presence of Uterine Irritability}
    \item{\code{FTV}}{Number of Physician Visits During the First Trimester}
    \item{\code{BWT}}{Birth Weight}
  }
}
\references{
Hosmer, D.W., and Lemeshow, S. (1989-2000). Applied Logistic Regression, 2e. J. Wiley.}
\examples{
data(lowbwt)
lowglm <- glm(LOW~AGE+LWT+RACE+FTV,data=lowbwt,family='binomial') 
lowglm$coefficients
}
\keyword{logistic regression, model selection}
