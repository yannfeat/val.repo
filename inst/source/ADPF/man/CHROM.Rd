\name{CHROM}
\alias{CHROM}
\docType{data}
\title{Data frame of Chromatogram values
}
\description{This file contains a \code{data.frame} of sample chromotography data. The 6th column is data without noise and the first five all have some gaussian noise added; these data sets showcase the advantages of ADPF over Savitzky-Golay.
}
\usage{data("CHROM")}
\format{
  A data frame with 201 observations on the following 6 variables.
  \describe{
    \item{\code{CHROM1}}{a numeric vector}
    \item{\code{CHROM2}}{a numeric vector}
    \item{\code{CHROM3}}{a numeric vector}
    \item{\code{CHROM4}}{a numeric vector}
    \item{\code{CHROM5}}{a numeric vector}
    \item{\code{CHROM6}}{a numeric vector}
  }
}

\source{
Barak, P., 1995. Smoothing and Differentiation by and Adaptive-Degree Polynomial filter; Anal. Chem. 67, 2758-2762.
}

\examples{
 ADPF::CHROM

smooth<-ADPF(CHROM[,6],0,9,13)
numpoints=length(CHROM[,6])
plot(x=1:numpoints,y=CHROM[,6]);lines(x=1:numpoints, y=smooth[,3])
}
\keyword{datasets}
