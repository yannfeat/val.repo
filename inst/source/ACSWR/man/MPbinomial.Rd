\name{MPbinomial}
\alias{MPbinomial}
\title{
Most Powerful Binomial Test
}
\description{
The function returns the level alpha MP test for the testing the hypothesis H:p=p0 against K:p=p_1 as ensured by the application of Neyman-Pearson lemma. 
}
\usage{
MPbinomial(Hp, Kp, alpha, n)
}
\arguments{
  \item{Hp}{
the value of p under hypothesis H}
  \item{Kp}{
the value of p under hypothesis K
}
  \item{alpha}{
size of the test
}
  \item{n}{
sample size
}
}
\author{
Prabhanjan N. Tattar
}
\seealso{
binom.test
}
\keyword{Most powerful test}
\keyword{binomial distribution}
