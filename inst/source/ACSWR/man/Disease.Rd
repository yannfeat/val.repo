\name{Disease}
\alias{Disease}
\docType{data}
\title{
Disease Outbreak Study
}
\description{
The purpose of this health study is investigation of an epidemic outbreak due to mosquitoes. A random sample from two sectors of the city among the individuals has been tested to determine if the individual had contracted the disease forming the binary outcome. 
}
\usage{data(Disease)}
\format{
  A data frame with 98 observations on the following 5 variables.
  \describe{
    \item{\code{x1}}{age}
    \item{\code{x2}}{socioeconomic status of three categories between \code{x2} and \code{x3}}
    \item{\code{x3}}{socioeconomic status of three categories between \code{x2} and \code{x3}}
    \item{\code{x4}}{sector of the city}
    \item{\code{y}}{if the individual had contracted the disease forming the binary outcome}
  }
}
\references{
Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (1974-2005). Applied
Linear Statistical Models, 5e. McGraw-Hill.
}
\examples{
data(Disease)
DO_LR <- glm(y~.,data=Disease,family='binomial')
LR_Residuals <- data.frame(Y = Disease$y,Fitted = fitted(DO_LR),
Hatvalues = hatvalues(DO_LR),Response = residuals(DO_LR,"response"), Deviance = 
residuals(DO_LR,"deviance"), Pearson = residuals(DO_LR,"pearson"), 
Pearson_Standardized = residuals(DO_LR,"pearson")/sqrt(1-hatvalues(DO_LR)))
LR_Residuals
}
\keyword{residual analysis, logistic regression}
