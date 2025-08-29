\name{siegel.tukey}
\alias{siegel.tukey}
\title{
Siegel-Tukey Nonparametric Test}
\description{
This function provided an implementation of the nonparametric discussed in "Section 8.5.3 The Siegel-Tukey Test". 
}
\usage{
siegel.tukey(x, y)
}
\arguments{
  \item{x}{
Values from Sample 1}
  \item{y}{
Values from Sample 2
}
}
\details{
For more details, refer Section 8.5.3 The Siegel-Tukey Test. }
\author{
Prabhanjan N. Tattar
}
\examples{
x <- c(0.028, 0.029, 0.011, -0.030, 0.017, -0.012, -0.027,-0.018, 0.022, -0.023)
y <- c(-0.002, 0.016, 0.005, -0.001, 0.000, 0.008, -0.005,-0.009, 0.001, -0.019)
siegel.tukey(x,y)
}
\keyword{ Siegel-Tukey  }
\keyword{Nonparametric Test }
