\encoding{UTF-8}
\name{Ake-package}
\alias{Ake-package}
\alias{Ake}
\docType{package}
\title{
 Associated kernel estimations
}
\description{
 Continuous and discrete estimation of density \code{\link{dke.fun}}, probability mass function (p.m.f.) \code{\link{kpmfe.fun}} and regression \code{\link{reg.fun}} functions are performed using  continuous and discrete associated kernels. The cross-validation technique \code{\link{hcvc.fun}}, \code{\link{hcvreg.fun}} and the Bayesian procedure \code{\link{hbay.fun}} are also implemented for bandwidth selection. 
}
\details{

\describe{
\item{The estimated density or p.m.f:}{ The associated kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined as

\deqn{\widehat{f}_n(x) = \frac{1}{n}\sum_{i=1}^{n}{K_{x,h}(X_i)},} where \eqn{K_{x,h}} is one of the kernels \code{\link{kef}} defined below.
 In practice, we first calculate the global normalizing constant


\deqn{{C}_n =   \int_{x\in T}{\widehat{f}_n(x) \nu(dx)},} where  \eqn{T}{T} is the support of the density or p.m.f. function and \eqn{ \nu}{\nu} is the Lebesgue or count measure on \eqn{T}{T}. For both continuous and discrete associated kernels, this normalizing constant is not generally equal to 1 and it will be computed. The represented  density  or p.m.f. estimate is then \eqn{\tilde{f}_n=\widehat{f}_n/C_n}{\tilde{f}_n=\widehat{f}_n/C_n}. 


For \bold{discrete data}, the integrated squared error (ISE) defined by \deqn{{ISE}_0 =   \sum_{x\in N}{{\{\tilde{f}_n(x)} - f_0(x)\}^2}} is the criteria used to measure the smoothness of the associated kernel estimator \eqn{\tilde{f}_n}{\tilde{f}_n} with the empirical p.m.f. \eqn{f_0}{f_0}; 
 see Kokonendji and Senga Kiessé (2011).

   }

}

\describe{
\item{The estimated regressor:}{ Both in continuous and discrete cases, considering the relation between a response variable \eqn{y}{y} and an explanatory variable \eqn{x}{x} given by \deqn{y=m(x)+\epsilon ,} where \eqn{m}{m} is an unknown regession function on \eqn{T}{T} and \eqn{\epsilon}{\epsilon} the disturbance term with null mean and finite variance. Let  \eqn{(x_1,y_1),\ldots,(x_n,y_n)} be a sequence of independent and identically distributed (iid) random vectors on \eqn{T\times R}{T\times R} with \eqn{m(x)=E(y|x)}.
The well-known Nadaraya-Watson estimator using  associated kernels is  \eqn{\widehat{m}_n}{\widehat{m}_n} defined as

\deqn{\widehat{m}_n(x) = \sum_{i=1}^{n}{\omega_{x}(X_i)Y_i},} where  \eqn{\omega_{x}(X_i)=K_{x,h}(X_i)/\sum_{i=1}^{n}{K_{x,h}(X_i)}} and \eqn{K_{x,h}} is one of the associated kernels defined below.
 
Beside the criterion of kernel support, we retain the root mean squared error (RMSE) and also the practical coefficient of determination \eqn{R^2}{R^2} defined  respectively by

\deqn{RMSE = \sqrt{\frac{1}{n}\sum_{i=1}^{n}{\{y_i-\widehat{m}_n(x_i)\}^2}}} and
\deqn{R^2=\frac{\sum_{i=1}^{n}{\{\widehat{m}_n(x_i)-\bar{y}\}^2}}{\sum_{i=1}^{n}(y_i-\bar{y})^2},}
where \eqn{\bar{y}=n^{-1}(y_1+\ldots+y_n)}; see Kokonendji et al. (2009).

   }

}

  	Given a data sample, the  package allows to compute the density or p.m.f. and regression functions using one of the seven associated kernels: extended beta, lognormal, gamma, reciprocal inverse Gaussian for continuous data, DiracDU for categorical data, and binomial and discrete triangular for count data. The bandwidth parameter is computed using the cross-validation technique. When the associated kernel function is binomial, the bandwidth parameter is also computed using the local Bayesian procedure. The associated kernel functions  are defined below. The first four kernels are for continuous data and the last three kernels are for discrete case. 

	\describe{
	           
	        \item{Extended beta kernel:}{ The extended beta kernel is defined on \eqn{{S}_{x,h,a,b}=[a,b]=T} with \eqn{a<b<\infty}, \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0}:

			 
\deqn{BE_{x,h,a,b}(y) = \frac {(y-a)^{(x-a)/\{(b-a)h\}}(b-y)^{(b-x)/\{(b-a)h\}}} {(b-a)^{1+h^{-1}}B\left(1+(x-a)/(b-a)h,1+(b-x)/(b-a)h\right)}1_{S_{x,h,a,b}}(y),}



where \eqn{B(r,s)=\int_0^1 t^{r-1}(1-t)^{s-1}dt} is the usual beta function with \eqn{r>0}, \eqn{s>0} and  \eqn{1_A}{1[A]} denotes the indicator function of A.  For \eqn{a=0} and \eqn{b=1}, it corresponds to the beta kernel which is the probability density function of the beta distribution with shape parameters \eqn{1+x/h} and \eqn{(1-x)/h}; see  Libengué (2013).
    }

  \item{Gamma kernel:}{ The gamma kernel is defined on \eqn{{S}_{x,h}=[0, \infty)=T} with \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0} by
 
   \deqn{GA_{x,h}(y) = \frac {y^{x/h}} {\Gamma(1+x/h)h^{1+x/h}}exp\left(-\frac{y}{h} \right)1_{S_{x,h}}(y),}
    
 where  \eqn{\Gamma(z)=\int_0^\infty t^{z-1}e^{-t}dt}  is the classical gamma function. The  probability density function \eqn{GA_{x,h}} is the gamma distribution with scale parameter \eqn{1+x/h} and shape parameter \eqn{h}; see Chen (2000).  
   }



 \item{Lognormal kernel:}{The lognormal kernel is defined on \eqn{{S}_{x,h}=[0,\infty)=T} with \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0} by
    

     \deqn{LN_{x,h}(y) = \frac {1} {yh\sqrt{2\pi}}exp\left\{-\frac{1}{2}\left(\frac{1}{h}log(\frac{y}{x})-h \right)^{2}\right\}1_{S_{x,h}}(y).}
    
 It is the probability density function  of the classical lognormal distribution with  parameters \eqn{log(x)+h^{2}} and \eqn{h}; see Libengué (2013).
   }


		    
	        \item{Binomial kernel:}{ Let \eqn{x\in  N:= \{0, 1, \ldots \}} and \eqn{{S}_x = \{0, 1, \ldots, x + 1\}}. The Binomial kernel is defined on the support \eqn{{S}_x} by
			 

\deqn{B_{x,h}(y) = \frac {(x+1)!} {y!(x+1-y)!}\left(\frac{x+h}{x+1}\right)^y\left(\frac{1-h}{x+1}\right)^{(x+1-y)}1_{S_{x}}(y),}


where \eqn{ h\in(0, 1]}. Note that \eqn{B_{x,h}}{B_[x,h]} is the p.m.f. of the binomial distribution with its number of trials \eqn{ x+1} and its success probability \eqn{(x+h)/(x+1)}; see Kokonendji and Senga Kiessé (2011).

    }

 
\item{Discrete triangular kernel:}{ For fixed arm \eqn{a\in  N}, we define \eqn{{S}_{x,a} = \{x-a,\ldots, x, \ldots, x + a\}}.  The discrete triangular kernel is defined on  \eqn{{S}_{x,a}} by
 \deqn{DT_{x,h;a}(y) = \frac {(a+1)^h - |y-x|^h} {P(a,h)}1_{S_{x,a}}(y),}
			 where \eqn{x\in  N},  \eqn{h>0} and \eqn{P(a,h)=(2a+1)(a+1)^h - 2(1+2^h+ \cdots +a^h)} is the normalizing constant. For \eqn{a=0}, the Discrete Triangular kernel \eqn{DT_{x,h;0}}{DT_[x,h;0]} corresponds to the Dirac kernel on \eqn{x}; see Kokonendji et al. (2007), and also Kokonendji and Zocchi (2010) for an asymmetric version of discrete triangular. 

    }
    

 \item{DiracDU kernel:}{ For fixed number of categories \eqn{c\in \{2,3,...\} }, we define  \eqn{{S}_{c} = \{0, 1, \ldots, c-1\}}. The DiracDU kernel is defined on \eqn{{S}_{c}} by 
      \deqn{DU_{x,h;c}(y) = (1 - h)1_{\{x\}}(y)+\frac {h} {c-1}1_{S_{c}\setminus\{x\}}(y),}
    
 where \eqn{x\in {S}_{c} } and  \eqn{ h\in(0, 1]}. See Kokonendji and Senga Kiessé (2011), and also Aitchison and Aitken (1976) for multivariate case.
   }
Note that the global normalizing constant is 1 for DiracDU.


\item{The bandwidth selection:}{ Two functions are implemented to select the bandwidth: cross-validation and local Bayesian procedure. The cross-validation technique is used for all the associated kernels both in density and regression; see Kokonendji and Senga Kiessé (2011). The local Bayesian procedure is implemented to select the bandwidth in the estimation of p.m.f. when using binomial kernel; see Zougab et al. (2014).

In the coming versions of the package, adaptive Bayesian procedure will be included for bandwidth selection in density estimation when using gamma kernel. A global Bayesian procedure will also be implemented for bandwidth selection in regression when using binomial kernel. 

   }   

  }


}
\author{

W. E. Wansouwé, S. M. Somé and C. C. Kokonendji

Maintainer: W. E. Wansouwé <ericwansouwe@gmail.com>

}
\references{

Aitchison, J. and Aitken, C.G.G. (1976). Multivariate binary discrimination by the kernel method, \emph{Biometrika}  \bold{63}, 413 - 420.


Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

Chen, S. X. (2000). Probability density function estimation using gamma kernels, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Demétrio, C.G.B. (2009). Appropriate kernel regression on a count explanatory variable and applications,
\emph{Advances and Applications in Statistics}  \bold{12},  99 - 125.


Libengue, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

 Zougab, N., Adjabi, S. and Kokonendji, C.C. (2014). Bayesian approach in nonparametric count regression with binomial kernel, \emph{ Communications in Statistics - Simulation and Computation }  \bold{43}, 1052 - 1063.


}

\keyword{ package }

