\name{show}
\alias{show}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to visualize iteration results
}
\description{
The show function provides access to user-defined visualization functions.
}
\usage{
show(monitorfunc, g, genfits, objective, x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{monitorfunc}{Monitoring function}
  \item{g}{Generation number}
  \item{genfits}{A matrix for fitness values}
  \item{objective}{Type of optimization. "min" or "max"}
  \item{x}{...}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
NA
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
 \code{\link{monprogress}}
}
\examples{
n = 100
genfits = matrix(NA, nrow=n, ncol=5)
genfits[1,3] = 50
objective = "max"
monitorfunc = monprogress
for(i in 1:(n-1)){
  g=i
  show(monitorfunc,  g=g, genfits=genfits, 
    objective=objective, x=NULL)
  genfits[g+1, 3] = genfits[g, 3] + runif(1, -2, 5)
}
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
