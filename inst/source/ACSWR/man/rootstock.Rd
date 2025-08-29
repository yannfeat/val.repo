\name{rootstock}
\alias{rootstock}
\docType{data}
\title{
Apple of Different Rootstock
}
\description{
The goal is to test if the mean vector of the four variables is same across 6 stratas of the experiment. 
}
\usage{data(rootstock)}
\format{
  A data frame with 48 observations on the following 5 variables.
  \describe{
    \item{\code{rootstock}}{Six different rootstocks}
    \item{\code{y1}}{trunk girth at 4 years}
    \item{\code{y2}}{extension growth at 4 years}
    \item{\code{y3}}{trunk girth at 15 years}
    \item{\code{y4}}{weight of tree above ground at 15 years}
  }
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.
}
\examples{
data(rootstock)
attach(rootstock)
rs <- rootstock[,1]
rs <- factor(rs,ordered=is.ordered(rs)) # Too important a step
root.manova <- manova(cbind(y1,y2,y3,y4)~rs)
summary(root.manova, test = "Wilks")
}
\keyword{manova}
