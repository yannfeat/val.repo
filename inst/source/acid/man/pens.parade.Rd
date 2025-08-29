\name{pens.parade}
\alias{pens.parade}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pen's Parade
%%  ~~function to do ... ~~
}
\description{ This function plots Pen's parade.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pens.parade(x, bodies = TRUE, feet = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observed incomes.
%%     ~~Describe \code{x} here~~
}
  \item{bodies}{ a logical value indicating whether lines, i.e. the bodies, should be drawn.
%%     ~~Describe \code{bodies} here~~
}
  \item{feet}{ a numeric value indicating where the lines originate.
%%     ~~Describe \code{feet} here~~
}
  \item{\dots}{ additional arguments passed to the plot function.
%%     ~~Describe \code{\dots} here~~
}
}
\references{ Atkinson, A.B. (1975): The Economics of Inequality, Cleardon Press, Oxford.
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
GB2sample<-rGB2(100,b.test,a.test,p.test,q.test)
pens.parade( GB2sample)
}