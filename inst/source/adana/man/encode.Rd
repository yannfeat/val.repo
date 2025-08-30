\name{encode}
\alias{encode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert from real number to binary number
}
\description{
The function encode converts a real number to a binary number with m digits between the given lower bound and upper bound.
}
\usage{
encode(real, lb, ub, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{real}{A real number}
  \item{lb}{Lower bound of real number}
  \item{ub}{Upper bound of real number}
  \item{m}{Number of the digits of output value.}
}
\details{
This function converts a real number to its binary equivalent expressed in m digits in the range [lb, ub].
}
\value{
Returns the binary equivalent of the input number.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{decode}}
}
\examples{
x = 102.5
encode(x, lb=50, ub=250, m=8)
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
