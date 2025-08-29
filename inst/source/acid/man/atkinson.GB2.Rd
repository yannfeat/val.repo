\name{atkinson.GB2}
\alias{atkinson.GB2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Atkinson Index for a Generalised Beta Distribution of Second Kind
%%  ~~function to do ... ~~
}
\description{ This function computes the Atkinson index (I(epsilon)) for Generalised Beta Distribution of Second Kind. 
The function is exact for the values epsilon=0, epsilon=1 and epsilon=2. 
For other values of epsilon, the function provides a numerical approximation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atkinson.GB2(b, a, p, q, epsilon = NULL, ylim = c(0, 1e+06), zeroapprox = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ the parameter b of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{b} here~~
}
  \item{a}{  the parameter a of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{a} here~~
}
  \item{p}{  the parameter p of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{p} here~~
}
  \item{q}{  the parameter q of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{q} here~~
}
  \item{epsilon}{ inequality aversion parameter as denoted by Atkinson (1970). The default is epsilon=1. 
%%     ~~Describe \code{epsilon} here~~
}
  \item{ylim}{ limits of the interval of y considered needed for the approximation of the entropy measure. The default is [0,1e+06].
%%     ~~Describe \code{ylim} here~~
}
  \item{zeroapprox}{ an approximation for zero needed for the approximation of the entropy measure. The default is 0.01.
%%     ~~Describe \code{zeroapprox} here~~
}
}
 \value{  returns the selected Atkinson inequality index.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\references{
Atkinson, A.B. (1970): On the Measurment of Inequality, in: Journal of Economic Theory, Vol. 2(3), pp. 244-263.

Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 87-166, Elsevier, Amsterdam.

}
\author{ Alexander Sohn
%%  ~~who you are~~
}
\seealso{ \code{\link[ineq]{ineq}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a.test<- 4
b.test<- 20000
p.test<- 0.7
q.test<- 1
epsilon.test<-1
GB2sample<-rGB2(1000,b.test,a.test,p.test,q.test)
atkinson.GB2(b.test,a.test,p.test,q.test,epsilon=epsilon.test,ylim=c(0,1e+07))
atkinson(GB2sample, epsilon.test)
}