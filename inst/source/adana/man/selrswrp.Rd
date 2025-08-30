\name{selrswrp}
\alias{selrswrp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random selection with replacement and proportion
}
\description{
Random selection is made by simple random sampling method with replacements, based on the fitness values of individuals. Each individual has the chance to be selected proportionally to their fitness value.
}
\usage{
selrswrp(fitvals, ns, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitvals}{Vector of fitness values belonging to individuals}
  \item{ns}{Number of individuals to be selected}
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
  \code{\link{seltrunc}},
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
  \code{\link{selboltour}},
  \code{\link{sellrs}},
  \code{\link{sellrs2}},
  \code{\link{sellrs3}},
  \code{\link{selnlrs}},
  \code{\link{selers}}  
  
}
\examples{
fitvals = c(6, 1, 2, 4, 5)              # Fitness Values
cnames = paste0("C",1:length(fitvals))  # Chromosome names
matpool = selrswrp(fitvals)
cat("Selected Chromosomes: ", cnames[matpool], "\n")
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
