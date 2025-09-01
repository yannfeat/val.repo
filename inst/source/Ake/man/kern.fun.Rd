\encoding{UTF-8}
\name{kern.fun}
\alias{kern.fun}
\alias{kern.fun.default}

\title{ The associated kernel function

}
\description{ The (S3) generic function \code{kern.fun} computes the value of the associated kernel function.
 Its default method does so with a given kernel 
  and bandwidth \eqn{h}.
}
\usage{
kern.fun(x, \dots)
\method{kern.fun}{default}(x, t, h, type_data = c("discrete", "continuous"),
 ker = c("bino", "triang", "dirDU", "BE", "GA", "LN", "RIG"), 
a0 = 0, a1 = 1, a = 1, c = 2, \dots)
}

\arguments{
  \item{x}{ The target

}
  \item{t}{ A single value or the grid  where the discrete associated kernel function is computed.

}
  \item{h}{ The bandwidth or smoothing parameter.

}
  \item{type_data}{ The sample data type

}
  \item{ker}{The associated kernel: "dirDU" DiracDU,"bino" Binomial, "triang" Discrete Triangular kernel, "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian.

}\item{a0}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
  \item{a1}{
The right bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
  \item{a}{
The arm in Discrete Triangular kernel. The default value is 1.
}
  \item{c}{
The number of categories in DiracDU kernel. The default value is 2.
}
\item{\dots}{
Further arguments
}
}
\details{
The associated kernel is one of the those which have been defined in the sections above : extended beta, gamma,lognormal, reciprocal inverse Gaussian, DiracDU, Binomial and Discrete Triangular; see Kokonendji and Senga Kiessé (2011), and also Kokonendji et al. (2007).
}
\value{
Returns the value of the discrete associated kernel function at \code{t} according to the target and the bandwidth.
}

\references{


Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
 \emph{ Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function,
 \emph{ Journal of Nonparametric Statistics }  \bold{19},  241 - 254.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji
}
\examples{
x<-5
h<-0.2
t<-0:10
kern.fun(x,t,h,"discrete","bino")
}
\keyword{smooth}
\keyword{nonparametric}
