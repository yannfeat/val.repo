\name{hardness}
\alias{hardness}
\docType{data}
\title{
Hardness and a Block Experiment
}
\description{
Four types of tip are used which form the blocks in this experiment. The variable of interest is the hardness which further depends on the type of metal coupon. For each type of the tip, the hardness is observed for 4 different types the metal coupon.
}
\usage{data(hardness)}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{Tip_Type}}{Four types of tip which form the blocks}
    \item{\code{Test_Coupon}}{Four different types of metal coupons}
    \item{\code{Hardness}}{Hardness of the coupon}
  }
}
\references{
Montgomery, D. C. (1976-2012). Design and Analysis of Experiments, 8e. J.Wiley.
}
\examples{
data(hardness)
hardness$Tip_Type <- as.factor(hardness$Tip_Type)
hardness$Test_Coupon <- as.factor(hardness$Test_Coupon)
hardness_aov <- aov(Hardness~Tip_Type+Test_Coupon,data=hardness)
summary(hardness_aov)
}
\keyword{block experiment}
