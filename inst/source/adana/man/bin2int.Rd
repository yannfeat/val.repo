\name{bin2int}
\alias{bin2int}
\title{Convert Binary Numbers to Integers}
\description{
The function bin2int converts a binary coded number to integer.
}
\usage{bin2int(bin)}
\arguments{
  \item{bin}{A binary coded number. }
}
\details{The bin2int function works as a compliment of the int2bin function.}

\value{Returns the integer equivalent of the input number.}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{int2bin}}
}
\examples{
x <- c(1,1,1,1,1,0,1,0,0)
bin2int(x)  # returns 500
}

\keyword{attribute}
\keyword{manip}

\concept{binary }
\concept{integer}
\concept{conversion of numbers}
