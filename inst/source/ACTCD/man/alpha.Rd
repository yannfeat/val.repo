\name{alpha}
\alias{alpha}

\title{
All possible attribute patterns
}
\description{
This function is used to generate all possible attribute patterns given the number of attributes.
}
\usage{
alpha(K)
}

\arguments{
  \item{K}{
The number of attributes.
}
}

\value{
A \eqn{2^K \times K} binary matrix is returned, 1 representing mastery of the attributes and 0 representing non-mastery of the attributes.
}

\seealso{
\code{\link{eta}}
}
\examples{
# Generate all possible attribute patterns given the number of attributes, K.
K <- 3
A3 <- alpha(K)
K <- 4
A4 <- alpha(K)
}
