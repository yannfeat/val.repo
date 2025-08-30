\name{selrscale2}
\alias{selrscale2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank Scaling 2
}
\description{
The Rank Scaling-2 is a selection method in which fitness values are scaled according to their ordinal number. Selection pressure can be adjusted by the user.
}
\usage{
selrscale2(fitvals, ns, sels, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{sels}{Scaling factor,  (\code{1.0 <= sels <= 2.0})}  
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
  \code{\link{select}},
  \code{\link{selrand}},
  \code{\link{selrswrp}},
  \code{\link{selrws}},
  \code{\link{selrws2}},
  \code{\link{selrss}},  
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selwscale}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},  
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}}     
}
\examples{
fitvals = c(6.1, 3.5, 6.2, 4.4, 5.2)
cnames = paste0("C",1:length(fitvals))
matpool = selrscale2(fitvals)
cat(cnames[matpool],"\n")
matpool = selrscale2(fitvals, sels=2)
cat(cnames[matpool],"\n")
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
