\name{raep}
\alias{raep}
\title{Simulating realizations from the asymmetric exponential power (AEP) distribution}
\description{Simulates realizations from AEP distribution.}
\usage{raep(n, alpha, sigma, mu, epsilon)}
\arguments{
\item{n}{Number of requested realizations.}
\item{alpha}{Tail thickness parameter.}
\item{sigma}{Scale parameter.}
\item{mu}{Location parameter.}
\item{epsilon}{Skewness parameter.}
}
\value{
A vector of length \code{n}, consists of the random generated values from AEP distribution.
}
\author{Mahdi Teimouri}
\examples{
raep(n = 100, alpha = 1.5, sigma = 1, mu = 0, epsilon = 0.5)
}
