\name{acss.data-package}
\alias{acss.data-package}
\docType{package}
\title{
Data Only: Algorithmic Complexity of Short Strings (Computed via Coding Theorem Method)
}
\description{
Data only package providing the algorithmic complexity of short strings, computed using the coding theorem method. For a given set of symbols in a string, all possible or a large number of random samples of Turing machines (TM) with a given number of states (e.g., 5) and number of symbols corresponding to the number of symbols in the strings were simulated until they reached a halting state or failed to end. This package contains data on 4.5 million strings from length 1 to 12 simulated on TMs with 2, 4, 5, 6, and 9 symbols. The complexity of the string corresponds to the distribution of the halting states of the TMs.
}
\details{
\tabular{ll}{
Package: \tab acss.data\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-04-02\cr
License: GPL (>= 2)\cr
URL: \tab \url{https://complexity-calculator.com/methodology.html}\cr
}
This package \emph{only} contains data. Therefore, this package is not intended to be used directly, but  through functions in package \pkg{acss}.
}
\author{
The data in this package was created by Fernando Soler Toscano, Nicolas Gauvrit, and Hector Zenil.\cr
Data was ported to R by Henrik Singmann.

Maintainer: Henrik Singmann <singmann@gmail.com>
}
\references{
Delahaye, J.-P., & Zenil, H. (2012). Numerical evaluation of algorithmic complexity for short strings: A glance into the innermost structure of randomness. \emph{Applied Mathematics and Computation}, 219(1), 63-77. doi:10.1016/j.amc.2011.10.006

Gauvrit, N., Zenil, H., Delahaye, J.-P., & Soler-Toscano, F. (in press). Algorithmic complexity for short binary strings applied to psychology: a primer. \emph{Behavior Research Methods}. doi:10.3758/s13428-013-0416-0

Soler-Toscano, F., Zenil, H., Delahaye, J.-P., & Gauvrit, N. (2012). \emph{Calculating Kolmogorov Complexity from the Output Frequency Distributions of Small Turing Machines}. arXiv:1211.1302 [cs.it].
}
\keyword{ package }
\keyword{datasets}
\seealso{
package \pkg{acss} for functions accessing this data.
}

