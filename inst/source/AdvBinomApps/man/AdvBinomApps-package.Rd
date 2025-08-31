\name{AdvBinomApps-package}
\alias{AdvBinomApps-package}
\alias{AdvBinomApps}
\docType{package}
\title{
Upper Clopper-Pearson Confidence Limits For Burn-in Studies under Additional Available Information
}
\description{
Functions to compute upper Clopper-Pearson confidence limits of early life failure probabilities and required sample sizes of burn-in studies under further available information, e.g. from other products or technologies. 
}
\details{
\tabular{ll}{
Package: \tab AdvBinomApps\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-04-05\cr
License: \tab GPL-3\cr
}
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}
\references{

C.J. Clopper and E.S. Pearson: \emph{The use of confidence or fiducial limits illustrated in the case of the binomial}. Biometrika, 26(4): 404-413, 1934.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Decision-Theoretical Model for Failures Tackled by Countermeasures}. IEEE Transactions on Reliability, 63(2): 583-592, 2014. DOI: 10.1109/TR.2014.2315952.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure probability estimation under additional subsystem information with application to semiconductor burn-in}. Resubmitted to: Journal of Applied Statistics, 2015.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{An Advanced Area Scaling Approach for Semiconductor Burn-in}. Microelectronics Reliability, 55(1): 129-137, 2015. DOI: 10.1016/j.microrel.2014.09.007.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure Probability Estimation with Differently Sized Reference Products for Semiconductor Burn-in Studies}. Applied Stochastic Models in Business and Industry, 31(5): 732-744, 2015. DOI: 10.1002/asmb.2100.

\emph{Acknowledgment}

The work has been performed in the project EPT300, co-funded by grants from Austria, Germany, Italy, The Netherlands and the ENIAC Joint Undertaking. This project is co-funded within the programme "Forschung, Innovation und Technologie fuer Informationstechnologie" by the Austrian Ministry for Transport, Innovation and Technology.
}

\seealso{
\code{\link[GenBinomApps]{GenBinomApps}}
}
\examples{
#MULTIPLE REFERENCE PRODUCTS:
k<-c(1,2)
n<-c(110000,138000)
A.ref<-c(5.21,10.71)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref(k,n,A.ref,A.follow,p.target=p.target)

#SYNERGIES
k<-c(0,1)
n<-c(110000,330000)
ci.syn(k,n,0.1,20e-06)

#SEPARATE AREA SCALING
k<-c(1,0)
n<-110000
A.ref<-c(1,1.5)
A.follow<-c(1,2)
p.target<-20e-06
ci.sas(k,n,A.ref,A.follow,0.1,p.target)
}
