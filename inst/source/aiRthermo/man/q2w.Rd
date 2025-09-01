\name{q2w}
\alias{q2w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Water vapour mixing Ratio to specific humidity
}
\description{
This function calculates the water vapour mixing ratio (kg/kg) from specific humidity (kg/kg).

}
\usage{
q2w(q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{
A vector with specific humidity values (kg/kg).

}
}
\value{
This function returns a vector with mixing ratio values in kg/kg.
}
\examples{
data(RadiosondeD)
dws<-RadiosondeD[,6]/1000
q2w(w2q(dws))
}
\keyword{Functions}