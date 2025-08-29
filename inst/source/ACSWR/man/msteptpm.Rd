\name{msteptpm}
\alias{msteptpm}
\title{
m-step Transition Probability Matrix Computation
}
\description{
The m-step transition probability matrix computation is provided in this function. The equation is based on the well-known "Chapman-Kolmogorov equation".
}
\usage{
msteptpm(TPM, m)
}
\arguments{
  \item{TPM}{
a transition probability matrix
}
  \item{m}{
the m step required
}
}
\author{
Prabhanjan N. Tattar
}

\examples{
EF2 <- Ehrenfest(2)
msteptpm(as.matrix(EF2),4)
}
\keyword{Transition Probability Matrix}
\keyword{m-step}
