\name{skewness.GB2}
\alias{skewness.GB2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Skewness of the Generalised Beta Distribution of Second Kind
%%  ~~function to do ... ~~
}
\description{ This function calculates the skewness of the Generalised Beta Distribution of Second Kind.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
skewness.GB2(b, a, p, q)
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
}
\value{ returns the skewness.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ Kleiber, C. and Kotz, S. (2003): Statistical Size Distributions in Economics and Actuarial Sciences, Wiley, Hoboken.
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
GB2sample<-rGB2(10000,b.test,a.test,p.test,q.test)
skewness.GB2(b.test,a.test,p.test,q.test)
#require(e1071)
#skewness(GB2sample)#note that this estimation is highly unstable even for larger sample sizes.
}