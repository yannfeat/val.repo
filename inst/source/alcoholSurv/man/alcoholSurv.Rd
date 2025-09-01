\name{alcoholSurv}
\alias{alcoholSurv}
\docType{data}
\title{
Light Alcohol Consumption and Survival
}
\description{
Data from 6 NHANES surveys with follow-up for mortality, as a matched
comparison of: (i) light daily alcohol, (ii) rare alcohol, and (iii)
no alcohol.
}
\usage{data("alcoholSurv")}
\format{
  A data frame with 5650 observations on the following 21 variables.
  \describe{
    \item{\code{SEQN}}{NHANES ID Number}
    \item{\code{nh}}{Identifies the NHANES years.  The data are from six NHANES, 2005-2006 to 2015-2016.}
    \item{\code{female}}{1=female, 0=male}
    \item{\code{age}}{Age in years at the time of the NHANES survey.}
    \item{\code{education}}{Level of education in five categories.  1 is less than 9th grade, 3 is high school, 5 is at least a BA degree.}
    \item{\code{hdl}}{HDL cholesterol in mg/dL}
    \item{\code{bmi}}{BMI or body-mass index}
    \item{\code{GH}}{Glycohemoglobin as a percent}
    \item{\code{smoke}}{Smoking status at the NHANES interview.  An ordered factor with levels \code{Everyday} < \code{Somedays} < \code{NotNow} < \code{Never}}
    \item{\code{z}}{Treatment indicator, 1 if consumes light daily alcohol, 0 if control.}
    \item{\code{gDrinks}}{Daily means: consumes light daily alcohol, between 1 and 3 drinks on at least 260=5x52 days each year.  Rare means rarely consumes alcohol.  None means consumed no alcohol in the past year.  An ordered factor with levels \code{Daily} < \code{Rare} < \code{None}}
    \item{\code{aDays}}{Days consumed alcohol in the past year.}
    \item{\code{aDrinks}}{Typical number of alcoholic drinks on drinking days.}
    \item{\code{a12life}}{1=consumed at least 12 alcoholic drinks in life, 0=other.  Based on NHANES question ALQ110.}
    \item{\code{aEverBinge}}{Was there ever a time in your life when you drank 5 or more drinks almost every day?  1=yes, 0=no.  The wording of this question changed slightly from one NHANES to another, sometimes asking about 4 drinks for a woman rather than 5.  See the NHANES documentation for details.}
    \item{\code{time}}{Time to death or censoring in months from the date of the
    NHANES examination.  Public data file using the National Death Index.}
    \item{\code{mortstat}}{Death/censoring indicator, 1=dead, 0=censored.}
    \item{\code{cod}}{Cause of death on the death certificate.  See codf.}
    \item{\code{codf}}{Cause of death as a factor.  An ordered factor with levels \code{Alive} < \code{Heart} < \code{Cancer} < \code{ChronicLung} < \code{Accident} < \code{Cerebrovascular} < \code{Alzheimer} < \code{Diabetes} < \code{FluPneumonia} < \code{Kidney} < \code{Other}}
    \item{\code{mset}}{Matched set indicator, 1 to 1130.}
    \item{\code{treated}}{The SEQN for the treated individual in a matched set.  Same information as mset, but in a different format.}
  }
}
\details{
This is a matched data set, one treated, 2 rare controls plus 2 none controls
in each of 1130 blocks of size 5.  See the description of the gDrinks variable above.  For details, see Rosenbaum (2025).
The examples below replicate analyses from Rosenbaum (2025).

The mortality data is from the public use linked morality files.  NHANES also has a restricted use version of the mortality files; it is not used here.
The public use file masks identity in various ways; see its web-page referenced below.
}
\source{
Data are from the NHANES webpage www.cdc.gov/nchs/nhanes/index.htm.

Also, 2019 Public-Use Linked Mortality Files are from
www.cdc.gov/nchs/data-linkage/mortality-public.htm
}
\references{
US National Health and Nutrition Examination Survey.
www.cdc.gov/nchs/nhanes/index.htm

Public-Use Linked Mortality Files.
www.cdc.gov/nchs/data-linkage/mortality-public.htm

Rosenbaum, P. R. (2025) <doi:10.1080/09332480.2025.2473291> Does a Daily Glass of Wine Lengthen Life? Insight from a Second Control Group. Chance, 38
(1), 25-30.
}
\examples{
#
# The example replicates results from Rosenbaum (2025)
#
oldpar <- par(no.readonly = TRUE)
data(alcoholSurv)
# Three treatment groups
table(alcoholSurv$gDrinks)
# In 1130 matched blocks of size 5
table(table(alcoholSurv$mset))
attach(alcoholSurv)
# Alcohol groups
table(gDrinks,aDays>0)
table(gDrinks,z)
table(gDrinks,aDrinks)
table(gDrinks,a12life)
table(gDrinks,aDays>24)
table(gDrinks,aDays>0)
# Alcohol groups are matched for covaiates
tapply(age,gDrinks,mean)
tapply(female,gDrinks,mean)
tapply(aEverBinge,gDrinks,mean)
tapply(education,gDrinks,mean)
prop.table(table(smoke,gDrinks),2)

library(survival)


par(bg="moccasin")

#  Make Figure 1
par(mfrow=c(1,3))
boxplot(age~gDrinks,las=1,cex.lab=1,cex.axis=1,xlab="Age",
        ylab="Age in Years")
axis(3,at=1:3,labels=round(tapply(age,gDrinks,mean)),cex.axis=1)
boxplot(education~gDrinks,las=1,cex.lab=1,cex.axis=1,xlab="Education",
        ylab="Education")
axis(3,at=1:3,labels=round(tapply(education,gDrinks,mean),2),cex.axis=1)

boxplot((aDays*aDrinks)~gDrinks,las=1,cex.lab=1,cex.axis=1,
        xlab="Alcoholic Drinks", ylab="Drinks Per Year")
axis(3,at=1:3,labels=round(tapply((aDays*aDrinks),gDrinks,mean)),cex.axis=1)

# Make Table 1

Female<-tapply(female,gDrinks,mean)*100
Age<-tapply(age,gDrinks,mean)
Education<-tapply(education,gDrinks,mean)
EverBinged<-tapply(aEverBinge,gDrinks,mean)*100
NeverSmoked<-tapply(smoke=="Never",gDrinks,mean)*100
NoLongerSmoke<-tapply(smoke=="NotNow",gDrinks,mean)*100
SmokeSomeDays<-tapply(smoke=="Somedays",gDrinks,mean)*100
SmokeEveryDay<-tapply(smoke=="Everyday",gDrinks,mean)*100
tabBal<-rbind(Female,Age,Education,EverBinged,NeverSmoked,NoLongerSmoke,SmokeSomeDays,
              SmokeEveryDay)
rm(Female,Age,Education,EverBinged,NeverSmoked,NoLongerSmoke,SmokeSomeDays,
   SmokeEveryDay)
tabBal2<-rbind(tabBal,prop.table(table(nh,gDrinks),2)*100)



# Make Figure 2

par(mfrow=c(1,2))

xlim<-c(0,150)  # Restrict plots to first 150 months, after which data are thin

coln<-c("blue","red","black")

st<-Surv(time,mortstat)
plot(survfit(st~(gDrinks=="Daily")),col=c("darkgreen","blue"),lty=c(4,1),lwd=2,ylim=c(.5,1),las=1,
     ylab="Probability of Survival",xlab="Months",cex.axis=.9,cex.lab=.9,
     main="(i)  All, I=1130, J=5", cex.main=.8,xlim=xlim)
legend(0.5,.63,c("Daily","Control"),col=c("blue","darkgreen"),lty=c(1,4),lwd=rep(2,2),cex=.8)

plot(survfit(st~gDrinks),col=coln,lty=1:3,lwd=2,ylim=c(.5,1),las=1,
     ylab="Probability of Survival",xlab="Months",cex.axis=.9,cex.lab=.9,
     main="(ii)  All, I=1130, J=5", cex.main=.8,xlim=xlim)
legend(0.5,.66,levels(gDrinks),col=coln,lty=1:3,lwd=rep(2,3),cex=.8)

# Make Figure 3

who<-smoke=="Never"
plot(survfit(st[who]~gDrinks[who]),col=coln,lty=1:3,lwd=2,ylim=c(.5,1),las=1,
     ylab="Probability of Survival",xlab="Months",cex.axis=.9,cex.lab=.9,xlim=xlim,
     main=paste("Never Smoker, I=",sum(z[who]),", J=5",sep=""),cex.main=.8)
legend(0.5,.66,levels(gDrinks),col=coln,lty=1:3,lwd=rep(2,3),cex=.8)

who<-(aEverBinge==0)
plot(survfit(st[who]~gDrinks[who]),col=coln,lty=1:3,lwd=2,ylim=c(.5,1),las=1,
     ylab="Probability of Survival",xlab="Months",cex.axis=.9,xlim=xlim,cex.lab=.9,
     main=paste("Never a Binge Drinker, I=",sum(z[who]),", J=5",sep=""),cex.main=.8)
legend(0.5,.66,levels(gDrinks),col=c(4,2,1),lty=1:3,lwd=rep(2,3),cex=.8)

rm(who)

#  Do formal analyses in footnote 1 using the Cox's stratified  proportional
#  hazards model

coxph(st~z+strata(treated))
confint(coxph(st~z+strata(treated)))
exp(confint(coxph(st~z+strata(treated))))

noDrinks<-1*(gDrinks=="None")
coxph(st~z+noDrinks+strata(treated))
confint(coxph(st~z+noDrinks+strata(treated)))
exp(confint(coxph(st~z+noDrinks+strata(treated))))
rm(coln,xlim,noDrinks)
detach(alcoholSurv)

par(oldpar)
}
\keyword{datasets}
\concept{Causal inference}
\concept{Second control group}
\concept{Two control groups}
\concept{Multiple control groups}
\concept{Observational study}
\concept{Evidence factors}
