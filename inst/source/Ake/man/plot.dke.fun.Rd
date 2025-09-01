\name{plot.dke.fun}
\alias{plot.dke.fun}

\title{
Plot of density function
}
\description{ The  \code{\link{plot.dke.fun}} is to plot the associated kernel density estimation.

}
\usage{
\method{plot}{dke.fun}(x,main = NULL, sub = NULL, xlab = NULL, 
ylab = NULL, type = "l", las = 1, lwd = 1, col = "blue", lty = 1, \dots)

}

\arguments{
  \item{x}{ An object class  \code{dke.fun}

}

  \item{main}{ The main parameter

}
  \item{sub}{ The sub title

}
  \item{xlab,ylab}{ The axis label

}
  \item{type}{ the type parameter

}
  \item{las}{ Numeric in \{0,1,2,3\}; the style of axis labels. 

}
  \item{lwd}{The line width, a positive number, defaulting to 1.

}
  \item{col}{ A specification for the default plotting color.

}
  \item{lty}{
The line type.
}
  \item{\dots}{
Futher arguments
}
}

\value{
Plot of associated kernel density function is sent to graphics window.
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

