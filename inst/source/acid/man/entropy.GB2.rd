\name{entropy.GB2}
\alias{entropy.GB2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Entropy Measures for a Generalised Beta Distribution of Second Kind
%%  ~~function to do ... ~~
}

\description{ This function computes four standard entropy measures from the generalised entropy class of inequality indices (I(alpha)) for Generalised Beta Distribution of Second Kind, 
namely the mean logarithmic deviation (I(0)), the Theil index (I(1)) as well as a bottom-sensitive index (I(-1)) and a top-sensitive index (I(2)). 
For other values of alpha, the function provides a numerical approximation.
 }
\usage{
entropy.GB2(b, a, p, q, alpha = NULL, ylim = c(0, 1e+06), zeroapprox = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ the parameter b of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{b} here~~
}
  \item{a}{ the parameter a of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{a} here~~
}
  \item{p}{ the parameter p of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{p} here~~
}
  \item{q}{ the parameter q of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{q} here~~
}
  \item{alpha}{ measure for the entropy measure as denoted by Cowell (2000). The default is alpha=1, i.e. the Theil Index. 
%%     ~~Describe \code{alpha} here~~
}
  \item{ylim}{ limits of the interval of y considered needed for the approximation of the entropy measure. The default is [0,1e+06].
%%     ~~Describe \code{ylim} here~~
}
  \item{zeroapprox}{ an approximation for zero needed for the approximation of the entropy measure. The default is 0.01.
%%     ~~Describe \code{zeroapprox} here~~
}
}
\value{ returns the selected entropy measure.

}
\references{ Kleiber, C. and Kotz, S. (2003): Statistical Size Distributions in Economics and Actuarial Sciences, Wiley, Hoboken.

Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 87-166, Elsevier, Amsterdam.

Jenkins, S.P. (2009): Distributionally-Sensitive Inequality Indices and the GB2 Income Distribution, in: Review of Income and Wealth, Vol. 55(2), pp.392-398.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

\examples{
a.test<- 4
b.test<- 20000
p.test<- 0.7
q.test<- 1
alpha.test<-1
GB2sample<-rGB2(1000,b.test,a.test,p.test,q.test)
entropy.GB2(b.test,a.test,p.test,q.test,alpha=alpha.test,ylim=c(0,1e+07))
entropy(GB2sample, alpha.test)
}

