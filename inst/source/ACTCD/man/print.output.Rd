\name{print.output}
\alias{print.cd.cluster}
\alias{print.labeling}
\alias{print.npar.CDM}

\title{
The function prints outputs obtained from the functions in the package.
}
\description{
Print outputs generated from the functions in the package.  
}
\usage{
\method{print}{cd.cluster}(x, \dots)
\method{print}{labeling}(x, \dots)
\method{print}{npar.CDM}(x, \dots)
}

\arguments{
  \item{x}{The output from the function (The list of all outputs).}
  \item{...}{Other arguments.}
}

\value{
  \item{cd.cluster}{The number of examinees within each cluster, membership based on cluster analysis and \eqn{\bm{W}} (See \code{\link{cd.cluster}} for details).}
  \item{labeling}{The estimated attribute profiles.}
  \item{npar.CDM}{The estimated attribute profiles.}
}

\seealso{
\code{\link{cd.cluster}}, \code{\link{labeling}},\code{\link{npar.CDM}}
}

