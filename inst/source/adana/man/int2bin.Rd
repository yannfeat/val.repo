\name{int2bin}
\alias{int2bin}
\title{Convert an integer to binary coded number}
\description{The function int2bin converts integers to binary coded numbers.}
\usage{int2bin(int, m)}
\arguments{
  \item{int}{Input number (integer)}
  \item{m}{Number of the digits of output value.}
}
\value{Returns the binary coded number of the integer number given in the input.}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{int2bin}}
}
\examples{
int2bin(250)      # returns 11111010
int2bin(250, 9)   # returns 011111010
}

\keyword{attribute }
\keyword{ manip }

\concept{binary }
\concept{integer}
\concept{conversion of numbers}
