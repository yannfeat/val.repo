\name{kpmfe.fun}
\alias{kpmfe.fun}
\alias{kpmfe.fun.default}

\title{
Function for associated kernel estimation of  p.m.f.
}
\description{
The function estimates the p.m.f. in a single value or in a
grid using discrete associated kernels. Three different associated kernels are available: DiracDU (for categorical data), binomial and discrete triangular (for count data).
}
\usage{
kpmfe.fun(Vec,\dots)
\method{kpmfe.fun}{default}(Vec, h, type_data = c("discrete", "continuous"), 
        ker = c("bino", "triang", "dirDU"), x = NULL, a = 1, c = 2, \dots)
}

\arguments{

\item{Vec}{
the data sample from which the estimate is to be computed.
}
  \item{h}{
The bandwidth or smoothing parameter. The smoothing bandwidth to be used, can also be a character 
          string giving a rule to choose the bandwidth.
}
  \item{type_data}{
The data sample type. Data type is "discrete" (categorical or count).
}
  \item{ker}{
The associated kernel: "dirDU" DiracDU,"bino" binomial, "triang" discrete triangular.
}
  \item{x}{
The points of the grid at which the density  is to be estimated.
}
   \item{a}{
The arm in discrete triangular kernel. The default value is 1.
}
  
  \item{c}{
The number of categories in DiracDU. The default value is 2.
}
  \item{\dots}{
Further arguments.
}
}

\details{ 
The associated  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined in the above sections.
We recall that in general, the sum of the estimated values on the support is not equal to 1. In practice, we compute the  global normalizing constant \eqn{C_n} before computing the estimated p.m.f. \eqn{\tilde{f}_n}{\tilde{f}_n}; see Kokonendji and Senga Kiessé (2011).

The bandwidth parameter in the function is obtained using the  cross-validation technique for the three associated kernels. For binomial kernel, the local Bayesian approach is also implemented and is recommanded to select the bandwidth; see Zougab et al. (2012).

}

\value{
Returns a list containing:
\item{data }{The number of observations.}
  \item{n }{The number of observations.}
 \item{eval.points}{The  support of the estimated p.m.f.}
\item{h }{The bandwidth}
 \item{C_n }{The global normalizing constant.}
 \item{ISE_0 }{The integrated square error.}
\item{f_0 }{ A vector of (x,f0(x)).}
\item{f_n }{A vector of (x,fn(x)).}
\item{f0 }{ The empirical p.m.f.}
\item{est.fn }{The estimated p.m.f. containing estimated values after normalization.}

}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function.
\emph{Journal of Nonparametric Statistics}  \bold{19},  241 - 254.

Zougab, N., Adjabi, S. and Kokonendji, C.C. (2012). Binomial kernel and Bayes local bandwidth in discrete functions estimation. 
\emph{Journal of Nonparametric Statistics}  \bold{24}, 783 - 795.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji
}

\examples{
## A sample data with n=60.
V<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,
7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,12,12,10,12,12,
13,14,15,16,16,17,0,12)


##The bandwidth can be the one obtained by cross validation.
h<-0.081
## We choose Binomial kernel.

est<-kpmfe.fun(Vec=V,h,"discrete","bino")
##To obtain the normalizing constant:
est


}

