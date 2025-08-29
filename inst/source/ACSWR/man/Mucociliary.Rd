\name{Mucociliary}
\alias{Mucociliary}
\docType{data}
\title{
Mucociliary Clearance
}
\description{
Table 6.1 of Hollander and Wolfe (1999) lists the data for Half-Time of Mucociliary Clearance. We need to test if the time across various treatments is equal or not.
}
\usage{data(Mucociliary)}
\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{Treatment}}{treatment levels \code{Asbestosis} \code{Normal Subjects} \code{Obstructive Airways Disease}}
    \item{\code{Time}}{half-time of mucociliary clearance}
  }
}
\references{
Hollander, M., and Wolfe, D. A. (1973-99). Nonparametric Statistical Methods, 2e. J. Wiley.
}
\examples{
data(Mucociliary)
Mucociliary$Rank <- rank(Mucociliary$Time)
aggregate(Mucociliary$Rank,by=list(Mucociliary$Treatment),sum)
kruskal.test(Time~Treatment,data=Mucociliary)
}
\keyword{Kruskal-Walis test}
