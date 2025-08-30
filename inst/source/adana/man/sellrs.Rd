\name{sellrs}
\alias{sellrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Ranking Selection 1
}
\description{
The Linear Ranking Selection operator selects via probabilities obtained using ordered numbers according to their fitness values (Pohlheim, 2020).
}
\usage{
sellrs(fitvals, ns, sels, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
  \item{sels}{Selection pressure,   (\code{1.0 <= sels <= 2.0})}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
The indices of randomly selected individuals are returned.
}
\references{
Pohlheim, H. (2020). Tutorial for "GEATbx: Genetic and Evolutionary Algorithms Toolbox for use with MATLAB", Version 3.30, URL http://www.geatbx.com/ver_3_3/algindex-02html#P181_11564.
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
  \code{\link{selrscale2}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}},
  \code{\link{selboltour}} 
}
\examples{
fitvals = c(6.1, 3.5, 6.2, 4.4, 5.2)    # Fitness Values
cnames = paste0("C",1:length(fitvals))  # Chromosome Names
matpool = sellrs(fitvals)
cat(cnames[matpool],"\n")
matpool = sellrs(fitvals, sels=2)
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
