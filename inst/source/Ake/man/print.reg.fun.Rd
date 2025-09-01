\name{print.reg.fun}
\alias{print.reg.fun}

\title{
Print for regression function
}
\description{
The function allows to print the result of computation in regression as a data frame.
}
\usage{
\method{print}{reg.fun}(x, digits = NULL, \dots)
}

\arguments{
  \item{x}{ object of class \code{reg.fun}.

}
  \item{digits}{ The number of digits

}
  \item{\dots}{ Further arguments

}
}
\details{ 
The  associated kernel estimator \eqn{\widehat{m}_n}{\widehat{m}_n} of  \eqn{m}{m} is defined in the above sections; see Kokonendji and Senga Kiessé (2011). The bandwidth parameter in the function is obtained using the cross-validation technique for the associated kernels. 

}
\value{
Returns a list containing:
 \item{data}{The explanatory variable, printed as a data frame }
\item{y}{The response variable, printed as a data frame }
 \item{n}{The size of the sample}
\item{kernel}{The associated kernel}
\item{h}{The smoothing parameter}
\item{eval.points}{The grid where the regression is computed, printed as data frame}
\item{m_n}{The estimated values, printed as data frame}
\item{Coef_det}{The Coefficient of determination}
 
}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Demétrio, C.G.B. (2009). Appropriate kernel regression on a count explanatory variable and applications,
\emph{Advances and Applications in Statistics}  \bold{12},  99 - 125.


 Zougab, N., Adjabi, S. and Kokonendji, C.C. (2014). Bayesian approach in nonparametric count regression with Binomial Kernel, \emph{ Communications in Statistics - Simulation and Computation }  \bold{43}, 1052 - 1063.

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
m_n<-reg.fun(x, y, "discrete",ker="bino", h)
print.reg.fun(m_n)
}
\keyword{print}
