\name{memory}
\alias{memory}
\docType{data}
\title{
Memory Recall Times
}
\description{
A test had been conducted with the purpose of investigating if people recollect pleasant memories associated with a word earlier than some unpleasant memory related with the same word. The word is flashed on the screen and the time an individual takes to respond via keyboard is recorded for both type of the memories.
}
\usage{data(memory)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Pleasant.memory}}{time to recollect pleasant memory}
    \item{\code{Unpleasant.memory}}{time to recollect unpleasant memory}
  }
}
\references{
Dunn, and Master. (1982). Obtained from 

\url{http://openlearn.open.ac.uk/mod/resource/view.php?id=165509}
}
\examples{
data(memory)
lapply(memory,fivenum)
lapply(memory,mad)
lapply(memory,IQR)
}
\keyword{fivenum, IQR}
