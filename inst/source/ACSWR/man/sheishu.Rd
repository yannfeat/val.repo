\name{sheishu}
\alias{sheishu}
\docType{data}
\title{
The Seishu Wine Study
}
\description{
The odor and taste of wines are recorded in a study. It is believed that the variables such as the pH concentration, alcohol content, total sugar, etc, explain the odor and taste of the wine.
}
\usage{data(sheishu)}
\format{
  A data frame with 30 observations on the following 10 variables.
  \describe{
    \item{\code{Taste}}{taste}
    \item{\code{Odor}}{odor}
    \item{\code{pH}}{pH concentration}
    \item{\code{Acidity_1}}{Acidity 1}
    \item{\code{Acidity_2}}{Acidity 2}
    \item{\code{Sake_meter}}{Sake meter}
    \item{\code{Direct_reducing_sugar}}{Direct reducing sugar}
    \item{\code{Total_sugar}}{Total sugar}
    \item{\code{Alcohol}}{type of alcohol}
    \item{\code{Formyl_nitrogen}}{Formyl nitrogen}
  }
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.
}
\examples{
data(sheishu)
noc <- c(2,3,3,2)
nov <- 10
v <- nrow(sheishu)-1
varsheishu <- var(sheishu)
s11 <- varsheishu[1:2,1:2]
s22 <- varsheishu[3:5,3:5]
s33 <- varsheishu[6:8,6:8]
s44 <- varsheishu[9:10,9:10]
u <- det(varsheishu)/(det(s11)*det(s22)*det(s33)*det(s44))
a2 <- nov^2 - sum(noc^2)
a3 <- nov^3 - sum(noc^3)
f <- a2/2
cc <- 1 - (2*a3 + 3*a2)/(12*f*v)
u1 <- -v*cc*log(u)
u; a2; a3; f; cc; u1
qchisq(1-0.001,37)
}
\keyword{testing independence of subvectors}
