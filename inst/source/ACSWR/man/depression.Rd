\name{depression}
\alias{depression}
\docType{data}
\title{
The Hamilton Depression Scale Factor
}
\description{
Hamilton depression scale factor IV is a measurement of mixed anxiety and depression and it is named after its inventor. In a double-blind experiment, this scale factor is obtained for 9 patients on their entry in a study, denoted by X. Post a tranquilizer T, the scale factor IV is again obtained for the same set of patients, which is denoted by Y. Here, an improvement due to tranquilizer T corresponds to a reduction in factor IV values.
}
\usage{data(depression)}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{Patient_No}}{Patient ID}
    \item{\code{X}}{measurement of depression at entry in a study}
    \item{\code{Y}}{measurement of depression post a tranquilizer}
  }
}
\references{
Sheshkin, D. J. (1997-2011). Handbook of Parametric and Nonparametric Statistical
Procedures, 5e. Chapman and Hall/CRC. 
}
\examples{
data(depression)
attach(depression)
names(depression)
wilcox.test(Y-X, alternative = "less")
wilcox.test(Y-X, alternative = "less",exact=FALSE,correct=FALSE)
}
\keyword{Wilcoxon test, Hamilton depression scale}
