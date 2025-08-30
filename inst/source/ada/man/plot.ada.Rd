\name{plot.ada}
\alias{plot.ada}

\title{
Plots for Ada
}

\description{
This function produces plots of the overall classification error at each 
boosting iteration for both the training and test sets.  
In addition, the function can produce plots of the measure of agreement (kappa) 
between the predicted classification and actual classification at each 
boosting iteration for both the training and test sets. 
}
\usage{
\method{plot}{ada}(x, kappa = FALSE, test=FALSE,cols= rainbow(dim(x$model$errs)[2]+1),tflag=TRUE, \dots)
}

\arguments{
  \item{x}{the object created by \code{ada}.}
  \item{kappa}{option for a plot of Kappa values at each iteration.
    kappa = TRUE produces a plot of Kappa values.  Default = FALSE.}
  \item{test}{option for a plot of testing error values at each iteration.
   test=TRUE produces a plot of test values.  Default=FALSE.}
  \item{cols}{colors used for lines to be plotted}
  \item{tflag}{inicates whether to include the tilte in the plot or not}
  \item{\dots}{additional \code{layout} command parameter (see \code{layout}). }
}

\value{
 No value returned
}

\seealso{\code{\link{ada}}}
\keyword{methods}
