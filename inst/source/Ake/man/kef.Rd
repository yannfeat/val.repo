\encoding{UTF-8}
\name{kef}
\alias{kef}

\title{
Continuous and discrete associated kernel function
}
\description{
This function computes the  associated kernel function. 
}
\usage{
kef(x, t, h, type_data = c("discrete", "continuous"), 
ker = c("bino", "triang", "dirDU", "BE", "GA", "LN", "RIG"), 
a0 = 0, a1 = 1, a = 1, c = 2)
}

\arguments{
  \item{x}{
The target.
}
  \item{t}{
A single value or the grid  where the  associated kernel function is computed.
}
  \item{h}{
The bandwidth or smoothing parameter.
}
  \item{type_data}{
The sample data type
}
  \item{ker}{
The associated kernel:"bino" Binomial, "triang" discrete triangular kernel, "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian,"dirDU" DiracDU.
}
  \item{a0}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
  \item{a1}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}
  \item{a}{
The arm in discrete triangular kernel. The default value is 1.
}
  \item{c}{
The number of categories in DiracDU kernel. The default value is 2.
}
}
\details{
The associated kernel is one of the those which have been defined in the sections above : extended beta, gamma, lognormal, reciprocal inverse Gaussian, DiracDU, binomial and discrete triangular; see Kokonendji and Senga Kiessé (2011), and also Kokonendji et al. (2007).
}
\value{
Returns the value of the associated kernel function at \code{t} according to the target and the bandwidth.
}
\references{


Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

Chen, S. X. (2000). Probability density function estimation using gamma kernels, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
 \emph{ Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function,
 \emph{ Journal of Nonparametric Statistics }  \bold{19},  241 - 254.


Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji
}
\examples{
x<-5
h<-0.2
t<-0:10
kef(x,t,h,"discrete","bino")
}
\keyword{smooth}
\keyword{nonparametric}
