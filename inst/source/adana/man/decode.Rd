\name{decode}
\alias{decode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert from binary number to real number
}
\description{
The function decode converts a binary number with m digits to a real number between the lower and upper bound.
}
\usage{
decode(bin, lb, ub, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bin}{A binary number}
  \item{lb}{Lower bound of real number}
  \item{ub}{Upper bound of real number}
  \item{m}{Number of the digits of output value.}  
}
\details{
This function converts a binary number with m digits to its real equivalent expressed in the range [lb, ub].
}
\value{
Returns the real equivalent of the input number.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{encode}}
}
\examples{
x = c(0,1,0,0,0,0,1,1)
decode(x, lb=50, ub=250, m=8)
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
