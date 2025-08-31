\name{plot.QRS}
\alias{plot.QRS}
\title{plot.QRS}
\description{By default, this function plots residuals from
the interrupted coefficient estimation selection model versus the corresponding fitted values. Alternatively, options to obtain a normal QQ plot or a scale-location plot of the residuals are also available.}
\usage{
\method{plot}{QRS}(x, normqq = FALSE, scaleloc = FALSE, ...)
}
\arguments{
  \item{x}{an object of QRS class}
  \item{normqq}{a logical value, if TRUE,  a normal QQ plot of the residuals will be plotted.} 
  \item{scaleloc}{a logical value, if TRUE,  a scale-location plot of the residuals will be plotted.} 
  \item{...}{arguments to be passed to plot methods, such as graphical parameters (see par). }
}
\value{No return value}
\author{Ladan Tazik, W.J. Braun}
\seealso{
\code{plot.lm}
}
\keyword{graphics}
