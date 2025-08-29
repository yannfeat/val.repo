\name{gini.Dag}
\alias{gini.Dag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gini Coefficient for the Dagum Distribution
%%  ~~function to do ... ~~
}
\description{ This function computes the Gini coefficient for the Dagum Distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini.Dag(a, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ the parameter a of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{a} here~~
}
  \item{p}{ the parameter p of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{p} here~~
}
}
\value{ returns the Gini coefficient.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.

%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{gini}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a.test<- 4
b.test<- 20000
p.test<- 0.7
alpha.test<-1
GB2sample<-rGB2(10000,b.test,a.test,p.test,1)
gini.Dag(a.test,p.test)
gini(GB2sample)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
