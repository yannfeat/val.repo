\name{WilsonCI}
\alias{WilsonCI}
\title{
Wilson Confidence Interval}
\description{
The Wilson confidence interval for a sample from binomial distribution is a complex formula. This function helps the reader in easily obtaining the required confidence interval as discussed and detailed in Section 16.5. }
\usage{
WilsonCI(x, n, alpha)
}
\arguments{
  \item{x}{
the number of successes
}
  \item{n}{
the number of trials
}
  \item{alpha}{
the confidence interval size
}
}

\author{
Prabhanjan N. Tattar
}
\examples{
WilsonCI(x=10658,n=15000,alpha=0.05)
prop.test(x=10658,n=15000)$conf.int
}
\keyword{Wilson confidence interval}
\keyword{binomial distribution}