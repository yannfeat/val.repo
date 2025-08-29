\name{SP}
\alias{SP}
\docType{data}
\title{
Understanding Strength of Paper with a Three Factorial Experiment
}
\description{
The strength of a paper depends on three variables: (i) the percentage of hardwood concentration in the raw pulp, (ii) the vat pressure, and (iii) the cooking time of the pulp. The hardwood concentration is tested at three levels of 2, 4, and 8 percentage, the vat pressure at 400, 500, and 650, while the cooking time is at 3 and 4 hours. For each combination of the these three factor variables, 2 observations are available, and thus a total of 3.3.2.2 = 36 observations. The goal of the study is investigation of the impact of the three factor variables on the strength of the paper, and the presence of interaction effect, if any.
}
\usage{data(SP)}
\format{
  A data frame with 36 observations on the following 4 variables.
  \describe{
    \item{\code{Hardwood}}{a factor with levels \code{2} \code{4} \code{8}}
    \item{\code{Pressure}}{a factor with levels \code{400} \code{500} \code{650}}
    \item{\code{Cooking_Time}}{a factor with levels \code{3} \code{4}}
    \item{\code{Strength}}{a numeric vector}
  }
}

\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(SP)
summary(SP.aov <- aov(Strength~.^3,SP))
}
\keyword{ANOVA}
