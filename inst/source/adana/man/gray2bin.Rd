\name{gray2bin}
\alias{gray2bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert gray code to binary integer #1
}
\description{
The function gray2bin converts gray coded integer to a binary coded number.
}
\usage{
gray2bin(gray)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gray}{ A gray coded number.}
}
\details{
The gray2bin function works as a compliment of the bin2gray function.
}
\value{
Returns the binary coded integer equivalent of the input number.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{bin2gray}},
 \code{\link{gray2bin2}}
}
\examples{
gray = c(1,1,1,0)
gray2bin(gray)       # returns 1011
gray = c(1,1,1,1)
gray2bin(gray)       # returns 1010
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
