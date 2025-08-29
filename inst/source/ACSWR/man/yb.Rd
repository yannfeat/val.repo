\name{yb}
\alias{yb}
\docType{data}
\title{
Youden and Beale's Data on Lesions of Half-Leaves of Tobacco Plant 
}
\description{
A simple and innovative design is often priceless. Youden and Beale (1934) sought to find the effect of two preparations of virus on tobacco plants. One half of a tobacco leaf was rubbed with cheesecloth soaked in one preparation of the virus extract and the second half was rubbed with the other virus extract. This experiment was replicated on just eight leaves, and the number of lesions on each half leaf was recorded.
}
\usage{data(yb)}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{Preparation_1}}{a numeric vector}
    \item{\code{Preparation_2}}{a numeric vector}
  }
}
\references{
Youden, W. J., and Beale, H. P. (1934). A Statistical Study of the Local Lesion Method for Estimating Tobacco Mosaic Virus. Contrib. Boyce Thompson Inst, 6, 437-454.
}
\examples{
data(yb)
summary(yb)
quantile(yb$Preparation_1,seq(0,1,.1)) # here seq gives 0, .1, .2, ...,1
quantile(yb$Preparation_2,seq(0,1,.1))
fivenum(yb$Preparation_1)
fivenum(yb$Preparation_2)
}
\keyword{exploratory data analysis}