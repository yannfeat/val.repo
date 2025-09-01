\name{alcoholSurv-package}
\alias{alcoholSurv-package}
\docType{package}
\title{
\packageTitle{alcoholSurv}
}
\description{
\packageDescription{alcoholSurv}
}
\details{
This package contains a dataframe, alcoholSurv, used to study the effects
of light daily alcohol on survival.  It also contains a function, wgtRankDef()
that conducts evidence factor analyses using decisive pairs.
}
\author{
\packageAuthor{alcoholSurv}

Maintainer: \packageMaintainer{alcoholSurv}
}
\references{
Rosenbaum, P. R. (2025a) <doi:10.1080/09332480.2025.2473291> Does a Daily Glass of Wine Lengthen Life? Insight from a Second Control Group. Chance, 38(1), 25-30.

Rosenbaum, P. R. (2025b) A new construction of evidence factors in an observational study of light daily alcohol consumption and longevity.
Manuscript.
}
\keyword{package}


\examples{
data(alcoholSurv)
# Three treatment groups
table(alcoholSurv$gDrinks)
# In 1130 matched blocks of size 5
table(table(alcoholSurv$mset))
}
