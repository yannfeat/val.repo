\name{reg.fun}
\alias{reg.fun}
\alias{reg.fun.default}


\title{Function for associated kernel estimation of regression

}
\description{The function estimates the discrete and continuous regression in a single value or in a
grid using associated kernels. Different associated  kernels are available: extended beta, gamma, lognormal, reciprocal inverse Gaussian (for continuous data), DiracDU (for categorical data), binomial and also discrete triangular (for count data).

}
\usage{
reg.fun(Vec, \dots)
\method{reg.fun}{default}(Vec, y, type_data = c("discrete", "continuous"), 
ker = c("bino", "triang", "dirDU", "BE", "GA", "LN", "RIG"),
 h, x = NULL, a0 = 0, a1 = 1, a = 1, c = 2, \dots)
}

\arguments{
  \item{Vec}{The explanatory variable.

}
  \item{y}{The response variable.

}
  \item{type_data}{ The sample data type.

}
  \item{ker}{ The associated kernel: "dirDU" DiracDU,"bino" binomial, "triang" discrete triangular, etc.

}
  \item{h}{ The bandwidth or smoothing parameter.

}
  \item{x}{The single value or the grid where the regression is computed.

}
  \item{a0}{The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.

}
  \item{a1}{The right bound of the support used for extended beta kernel. Default value is 0 for beta kernel.

}
  \item{a}{The arm in Discrete Triangular kernel. The default value is 1.

}
  \item{c}{The number of categories in DiracDU. The default value is 2.

}
  \item{\dots}{
Further arguments
}
}
\details{ 
The associated kernel estimator \eqn{\widehat{m}_n}{\widehat{m}_n} of  \eqn{m}{m} is defined in the above sections; see also Kokonendji and Senga Kiessé (2011). The bandwidth parameter in the function is obtained using the cross-validation technique for the seven associated kernels. For binomial kernel, the local Bayesian approach is also implemented; see Zougab et al. (2014).

}
\value{
Returns a list containing:
 \item{data}{The data sample, explanatory variable}
\item{y}{The data sample, response variable}
 \item{n}{The size of the sample}
\item{kernel}{The asociated kernel}
\item{h}{The bandwidth}
\item{eval.points}{The grid where the regression is computed}
\item{m_n}{The estimated values}
\item{Coef_det}{The coefficient of determination}
 
}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Demétrio, C.G.B. (2009). Appropriate kernel regression on a count explanatory variable and applications,
\emph{Advances and Applications in Statistics}  \bold{12},  99 - 125.


 Zougab, N., Adjabi, S. and Kokonendji, C.C. (2014). Bayesian approach in nonparametric count regression with binomial kernel, \emph{ Communications in Statistics - Simulation and Computation }  \bold{43}, 1052 - 1063.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji 
}

\examples{
data(milk)
x=milk$week
y=milk$yield
##The bandwidth is the one obtained by cross validation.
h<-0.10
## We choose binomial kernel.
\dontrun{
m_n<-reg.fun(x, y, "discrete",ker="bino", h)
}


}

