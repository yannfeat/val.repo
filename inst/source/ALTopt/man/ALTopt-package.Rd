\name{ALTopt-package}
\alias{ALTopt-package}

\docType{package}
\title{
\packageTitle{ALTopt}
}
\description{
\packageDescription{ALTopt}
}
\details{
\tabular{ll}{
Package: \tab ALTopt\cr
Version: \tab 0.1.2\cr
Authors@R: \tab as.person(c(
    "Kangwon Seo <seoka@missouri.edu> [aut, cre]",
    "Rong Pan <rong.pan@asu.edu> [aut]"
    ))\cr
Depends: \tab R (>= 3.0.0)\cr
License: \tab GPL-3\cr
LazyData: \tab true\cr
Imports: \tab cubature (>= 1.0),
lattice (>= 0.20),
methods\cr
Built: \tab R 3.6.1; ; 2019-12-12 12:30:00 UTC; windows\cr
}

Index:
\preformatted{
alteval.ic              Design evaluation with interval censoring.
alteval.rc              Design evaluation with right censoring.
altopt.ic               Optimal design with interval censoring.
altopt.rc               Optimal design with right censoring.
compare.fus             Comparing designs using FUS
compare.vdus            Comparing designs using VDUS
convert.stress.level    Coding and decoding stress level
design.plot             Design plot.
pv.contour.ic           Contour plot of prediction variance for a
                        design with interval censoring.
pv.contour.rc           Contour plot of prediction variance for a
                        design with right censoring.
pv.fus.ic               FUS (Fraction of Use Space) plot for interval
                        censoring.
pv.fus.rc               FUS (Fraction of Use Space) plot for right
                        censoring.
pv.vdus.ic              VDUS (Variance Dispersion of Use Space) plot
                        for interval censoring.
pv.vdus.rc              VDUS (Variance Dispersion of Use Space) plot
                        for right censoring.
}
}

\author{
Kangwon Seo [aut, cre],
  Rong Pan [aut]

Maintainer: Kangwon Seo <seoka@missouri.edu>
}
\references{
Seo, K. and Pan, R. (2015) ALTopt: An R Package for Optimal Experimental Design of Accelerated Life Testing. \emph{The R Journal}, \bold{7(2)}, 177-188.

Monroe, E. M., Pan, R., Anderson-Cook, C. M., Montgomery, D. C. and Borror C. M. (2011) A Generalized Linear Model Approach to Designing Accelerated Life Test Experiments, \emph{Quality and Reliability Engineering International} \bold{27(4)}, 595--607

Yang, T., Pan, R. (2013) A Novel Approach to Optimal Accelerated Life Test Planning With Interval Censoring, \emph{Reliability, IEEE Transactions on} \bold{62(2)}, 527--536
}

\concept{Accelerated Life Tests}
\concept{Optimal Design}
\seealso{
\code{\link{altopt.rc}, \link{altopt.ic}, \link{alteval.rc}, \link{alteval.ic}, \link{pv.contour.rc}, \link{pv.contour.ic}, \link{pv.fus.rc}, \link{pv.fus.ic}, \link{pv.vdus.rc}, \link{pv.vdus.ic}, \link{compare.fus}, \link{compare.vdus},
\link{design.plot}, \link{convert.stress.level}}
}
\examples{
\donttest{
# D optimal design of two stress factors with right censoring.
Design.D <- altopt.rc("D", 100, 100, 2, 1, formula = ~ x1 + x2 + x1:x2,
coef = c(0, -4.086, -1.476, 0.01))

design.plot(Design.D$opt.design.rounded, x1, x2)

pv.contour.rc(Design.D$opt.design.rounded, x1, x2, 100, 2, 1,
formula = ~ x1 + x2 + x1:x2, coef = c(0, -4.086, -1.476, 0.01),
useCond = c(1.758, 3.159))

FUS.D <- pv.fus.rc(Design.D$opt.design.rounded, 100, 2, 1,
formula = ~ x1 + x2 + x1:x2, coef = c(0, -4.086, -1.476, 0.01),
useLower = c(1.458, 2.859), useUpper = c(2.058, 3.459))

# U optimal design of two stress factors with right censoring.
Design.U <- altopt.rc("U", 100, 100, 2, 1, formula = ~ x1 + x2 + x1:x2,
coef = c(0, -4.086, -1.476, 0.01), useCond = c(1.758, 3.159))

design.plot(Design.U$opt.design.rounded, x1, x2)

pv.contour.rc(Design.U$opt.design.rounded, x1, x2, 100, 2, 1,
formula = ~ x1 + x2 + x1:x2, coef = c(0, -4.086, -1.476, 0.01),
useCond = c(1.758, 3.159))

FUS.U <- pv.fus.rc(Design.U$opt.design.rounded, 100, 2, 1,
formula = ~ x1 + x2 + x1:x2, coef = c(0, -4.086, -1.476, 0.01),
useLower = c(1.458, 2.859), useUpper = c(2.058, 3.459))

# Comparing D and U optimal design.
compare.fus(FUS.D, FUS.U)
}
}
