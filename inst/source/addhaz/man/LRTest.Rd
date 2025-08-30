\name{LRTest}
\alias{LRTest}

\title{Perform likelihood ratio test}

\description{
This function performs the likelihood ratio test to compare two nested binomial or multinomial additive hazard models. It can be used for model selection.
}

\usage{
LRTest(model1, model2)
}

\arguments{
  \item{model1, model2}{objects of class "binaddhazmod" or "multaddhazmod" to be compared. See example.}
}

\details{
 The likelihood ratio test is defined as -2*log(likelihood model 1/likelihood model 2).The resulting test statistic is assumed to follow a chi-squared distribution, with degrees of freedom (df) equal to the difference of the df between the models. If the test is statistically significant, the model with more variables fits the data significantly better than the model with less variables.
}

\value{
  A data frame with columns:
  \item{Res.df}{degrees of freedom for each model.}
  \item{Res.dev}{2*log-likelihood of each model.}
  \item{df}{difference in the degrees of freedom between models 1 and 2.}
  \item{Deviance}{difference between the 2*log-likelihood of models 1 and 2, representing the value of the likelihood ratio test statistic.}
  \item{Pr(>Chi)}{p-value, based on the chi-squared distribution.}
}

\examples{
  data(disabData)

  ## Comparing two binomial models

  fit1 <- BinAddHaz(dis.bin ~ diab + arth + stro , data = disabData, weights = wgt,
                    attrib = FALSE)

  diseases <- as.matrix(disabData[,c("diab", "arth", "stro")])
  fit2 <- BinAddHaz(dis.bin ~ factor(age) -1 + diseases:factor(age), data = disabData,
                    weights = wgt, attrib = FALSE)

  LRTest(fit2, fit1)

  ## Comparing two multinomial models
\dontrun{
  fit3 <- MultAddHaz(dis.mult ~ diab + arth + stro , data = disabData, weights = wgt,
                     attrib = FALSE)
  fit4 <- MultAddHaz(dis.mult ~ factor(age) -1 + diseases: factor(age), data = disabData,
                     weights = wgt, attrib = FALSE)
  LRTest(fit4, fit3)}
}

\keyword{ ~models }
\keyword{ ~regression }

