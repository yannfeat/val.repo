\name{adjectives}
\alias{adjectives}
\docType{data}
\title{
A Dataset for Factor Analysis}
\description{
The data set is obtained from Rencher (2002). Here, a 12-year old girl rates 7 of her acquaintances on a differential grade of 1-9 for five adjectives kind, intelligent, happy, likable, and just.
}
\usage{data(adjectives)}
\format{
  A data frame with 7 observations on the following 6 variables.
  \describe{
    \item{\code{People}}{a factor with levels \code{FATHER} \code{FSM1a} \code{FSM2} \code{FSM3} \code{MSMb} \code{SISTER} \code{TEACHER}}
    \item{\code{Kind}}{a numeric vector}
    \item{\code{Intelligent}}{a numeric vector}
    \item{\code{Happy}}{a numeric vector}
    \item{\code{Likeable}}{a numeric vector}
    \item{\code{Just}}{a numeric vector}
  }
}
\references{
Rencher, A.C. (2002). Methods of Multivariate Analysis, 2e. J. Wiley.}
\examples{
data(adjectives)
adjectivescor <- cor(adjectives[,-1])
round(adjectivescor,3)
adj_eig <- eigen(adjectivescor)
cumsum(adj_eig$values)/sum(adj_eig$values)
adj_eig$vectors[,1:2]
loadings1 <- adj_eig$vectors[,1]*sqrt(adj_eig$values[1])
loadings2 <- adj_eig$vectors[,2]*sqrt(adj_eig$values[2])
cbind(loadings1,loadings2)
communalities <- (adj_eig$vectors[,1]*sqrt(adj_eig$values[1]))^2+
(adj_eig$vectors[,2]*sqrt(adj_eig$values[2]))^2
round(communalities,3)
specific_variances <- 1-communalities
round(specific_variances,3)
var_acc_factors <- adj_eig$values
round(var_acc_factors,3)
prop_var <- adj_eig$values/sum(adj_eig$values)
round(prop_var,3)
cum_prop <- cumsum(adj_eig$values)/sum(adj_eig$values)
round(cum_prop,3)
}
\keyword{factor analysis}
