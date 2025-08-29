\name{cork}
\alias{cork}
\docType{data}
\title{
The Cork Dataset
}
\description{
Thickness of cork borings in four directions of North, South, East, and West are measured for 28 trees. The problem here is to examine if the bark deposit is same in all the directions.
}
\usage{data(cork)}
\format{
  A data frame with 28 observations on the following 4 variables.
  \describe{
    \item{\code{North}}{thickness of cork boring in the North direction}
    \item{\code{East}}{thickness of cork boring in the East direction}
    \item{\code{South}}{thickness of cork boring in the South direction}
    \item{\code{West}}{thickness of cork boring in the West direction}
  }
}
\references{
Rao, C. R. (1973). Linear Statistical Inference and Its Applications, 2e. J. Wiley.
}
\examples{
data(cork)
corkcent <- cork*0
corkcent[,1] <- cork[,1]-mean(cork[,1])
corkcent[,2] <- cork[,2]-mean(cork[,2])
corkcent[,3] <- cork[,3]-mean(cork[,3])
corkcent[,4] <- cork[,4]-mean(cork[,4])
corkcentsvd <- svd(corkcent)
t(corkcentsvd$u)\%*\%corkcentsvd$u
t(corkcentsvd$v)\%*\%corkcentsvd$v
round(corkcentsvd$u \%*\% diag(corkcentsvd$d) \%*\% t(corkcentsvd$v),2)
round(corkcent,2)
corkcentsvd$d
}
\keyword{multivariate dataset, singular value decomposition, cork dataset}
