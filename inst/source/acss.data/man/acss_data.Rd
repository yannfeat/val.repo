\docType{data}
\name{acss_data}
\alias{acss_data}
\title{acss_data: algorithmic complexity of short strings}
\usage{acss_data}
\format{
  A data frame with 4590267 observations on the following 5 variables.
  \describe{
    \item{\code{K.2}}{acss with 2 symbols, computed on all possible Turing machines (TM) with 5 states and 2 symbols.}
    \item{\code{K.4}}{acss with 4 symbols, computed on a large number of TMs with 4 states and 4 symbols.}
    \item{\code{K.5}}{acss with 5 symbols, computed on a large number of TMs with 4 states and 5 symbols.}
    \item{\code{K.6}}{acss with 6 symbols, computed on a large number of TMs with 4 states and 6 symbols.}
    \item{\code{K.9}}{acss with 9 symbols, computed on a large number of TMs with 4 states and 9 symbols.}
  }
}
\description{
Contains the algorithmic complexity for short string, an
approximation of the Kolmogorov Complexity of a short
string using the coding theorem method. For a given set of symbols in a string, all possible or a large number of random samples of Turing machines (TM) with a given number of states and number of symbols corresponding to the number of symbols in the strings were simulated until they reached a halting state or failed to end. The complexity of the string corresponds to the distribution of the halting states of the TMs.

See \url{https://complexity-calculator.com/methodology.html} for more information or references below.

This dataset shouldn't be called directly but rather
through the accessor functions in package \pkg{acss}.
}
\author{
Fernando Soler Toscano, Nicolas Gauvrit, and Hector Zenil.\cr
Ported to R by Henrik Singmann.
}
\source{
\url{https://complexity-calculator.com/methodology.html}
}
\references{
Delahaye, J.-P., & Zenil, H. (2012). Numerical evaluation of algorithmic complexity for short strings: A glance into the innermost structure of randomness. \emph{Applied Mathematics and Computation}, 219(1), 63-77. doi:10.1016/j.amc.2011.10.006

Gauvrit, N., Zenil, H., Delahaye, J.-P., & Soler-Toscano, F. (in press). Algorithmic complexity for short binary strings applied to psychology: a primer. \emph{Behavior Research Methods}. doi:10.3758/s13428-013-0416-0

Soler-Toscano, F., Zenil, H., Delahaye, J.-P., & Gauvrit, N. (2012). \emph{Calculating Kolmogorov Complexity from the Output Frequency Distributions of Small Turing Machines}. arXiv:1211.1302 [cs.it].

\url{https://complexity-calculator.com/}
}
\keyword{datasets}

