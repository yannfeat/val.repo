\name{w2q}
\alias{w2q}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Specific Humidity from mixing ratio
}
\description{
This function calculates the specific humidity from a given Water mixing ratio.
}
\usage{
w2q(w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
A vector with mixing ratio values in kg/kg.
}
}

\value{
The function returns a vector with the specific humidity.
}
\examples{
data(RadiosondeD)
dws<-RadiosondeD[,6]/1000
w2q(dws)
}
\keyword{Functions}