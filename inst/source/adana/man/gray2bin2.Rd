\name{gray2bin2}
\alias{gray2bin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert gray code to binary integer #2
}
\description{
The function gray2bin2 converts a gray-coded integer to a binary-coded number. 
The conversion function is performed according to the algorithm given by Chakraborty and Janikov (2003).
}
\usage{
gray2bin2(gray)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gray}{A gray coded number.}
}
\details{
To convert gray coded numbers to binary numbers, a conversion function is defined using the algorithm given by Chakraborty and Janikov (2003). This function is a generic function that does not use the xor operator.
}
\value{Returns the binary coded integer equivalent of the input number.}
\references{
Chakraborty, U.K., and Janikow C.Z. (2003). An analysis of Gray versus binary encoding in genetic search. Information Sciences, 156 (3-4), 253-269.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{bin2gray}},
 \code{\link{gray2bin}}
}
\examples{
gray = c(1,1,1,0) 
gray2bin2(gray)       # returns 1011

gray = c(1,1,1,1)
gray2bin2(gray)       # returns 1010

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
