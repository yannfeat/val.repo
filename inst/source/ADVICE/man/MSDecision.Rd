\name{MSDecision}
\alias{MSDecision}
\docType{data}
\title{Multiple Sclerosis Decision Delay}
\description{
This data frame contains the time (in weeks) between the initial
symptoms (onset symptoms) and the decision time to visit a doctor
in the case of 54 patients who eventually were diagnosed with multiple
sclerosis. Interest centers on whether there are any factors which 
tend to be related to the delay time.    
}
\usage{data(MSDecision)}
\format{
  A data frame with 54 observations on the following 16 variables.
  \describe{
\item{\code{Delay}}{numeric, time in weeks}
\item{\code{ClinicalDiseaseCourse}}{factor, 2 levels}   
\item{\code{CodedGender}}{factor, 2 levels, 1 = Male, 2 = Female}        
\item{\code{AgeAtOnset}}{numeric, age in years}
\item{\code{OnsetSymptom1}}{factor, 4 levels}
\item{\code{OnsetSymptom2}}{factor, 5 levels}
\item{\code{OnsetSymptomSeverity}}{factor, 2 levels, 0 = Low, 1 = High}
\item{\code{TriggerSymptom1}}{factor, 4 levels}         
\item{\code{TriggerSymptom2}}{factor, 4 levels}
\item{\code{TriggerSymptomSeverity}}{factor, 2 levels, 0 = Low, 1 = High}
\item{\code{FamilyHistory}}{factor, 2 levels, yes = there is MS in the family history}
\item{\code{FearOfWorseningSymptoms}}{factor, 2 levels}
\item{\code{MoreThanOneSymptom}}{factor, 2 levels}
\item{\code{EffectonResponsibilities}}{factor, 2 levels, yes = the symptoms are having an effect on the individual}
\item{\code{UncertainResponse}}{logical, TRUE = recorded delay time is not accurate}
  }
}
\details{The levels of the Clinical Disease Course variable are:
Clinically Isolated Syndrome and Relapse-Remitting.  
}
\examples{
   xy <- MSDecision
   xy$sensoryOnset1 <- factor(xy$OnsetSymptom1=="SENSORY")
   xy$brainstemOnset2 <- factor(xy$OnsetSymptom2=="BRAINSTEM")
   xy$sensoryTrigger1 <- factor(xy$TriggerSymptom1=="SENSORY")
   xy$brainstemTrigger2 <- factor(xy$TriggerSymptom2=="BRAINSTEM")
   xy <- xy[, -c(5, 6, 8, 9, 15)]
   xy[,1]<-log(xy[,1])
   names(xy)[1] <- "y"
   out <- ices(y ~ ., data = xy)
   summary(out)
   plot(out) 
   plot(out, normqq=TRUE)
   plot(out, scaleloc=TRUE)
}
\keyword{datasets}
