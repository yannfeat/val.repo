\name{bin2gray}
\alias{bin2gray}
\title{Convert from binary to gray code integer}
\description{
  The function bin2gray converts a binary coded number to gray coded integer.
}
\usage{bin2gray(bin)}
\arguments{
  \item{bin}{A binary coded number. }
}
\details{The bin2gray function works as a compliment of the gray2bin function.}

\value{Returns the gray coded integer equivalent of the input number.}
\author{
  Zeynel Cebeci & Erkut Tekeli
}
\seealso{
  \code{\link{gray2bin}}
}
\examples{
  bin = c(1,0,1,1)
  bin2gray(bin)     # returns 1110
  
  bin = c(1,0,1,0)
  bin2gray(bin)     # returns 1111
}

\keyword{attribute}
\keyword{manip}

\concept{binary }
\concept{integer}
\concept{conversion of numbers}
