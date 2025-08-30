\name{OrderComparisons}
\alias{OrderComparisons}
\alias{eq}
\alias{ne}
\alias{ge}
\alias{gt}
\alias{le}
\alias{lt}
\title{Functions to perform simple order comparisons}
\description{
  These functions perform simple order comparisons for two arguments, dealing with the machine inaccuracy for floating point arithmetics.
}
\usage{
eq(x, y, tol = .Machine$double.eps^0.5)
ne(x, y, tol = .Machine$double.eps^0.5)
ge(x, y, tol = .Machine$double.eps^0.5)
gt(x, y, tol = .Machine$double.eps^0.5)
le(x, y, tol = .Machine$double.eps^0.5)
lt(x, y, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{first argument (must be a numeric scalar)}
  \item{y}{second argument (must be a numeric scalar)}
  \item{tol}{comparison tolerance; differences smaller than \code{tol} are not considered.}
}
\details{
  When comparing two numeric scalars (e.g., for equality), machine inaccuracy can be the source of obviously erroneous results. These functions perform binary order comparisons that are tolerant towards machine inaccuracy, as an alternative to the standard comparators \code{==}, \code{!=}, \code{>=}, \code{>}, \code{<=} and \code{<}.
}
\value{
  The functions return a logical \code{TRUE} if their condition holds, and a logical \code{FALSE} otherwise.

  \code{eq(x, y)} checks whether \code{x} is equal to \code{y}

  \code{ne(x, y)} checks whether \code{x} is not equal to \code{y}

  \code{ge(x, y)} checks whether \code{x} is greater than or equal to \code{y}

  \code{gt(x, y)} checks whether \code{x} is greater than \code{y}

  \code{le(x, y)} checks whether \code{x} is less than or equal to \code{y}

  \code{lt(x, y)} checks whether \code{x} is less than \code{y}
}
\author{Marc Vandemeulebroecke}
\note{
  These functions cannot be used in a vectorized fashion.
}
\seealso{\code{\link{identical}}, \code{\link{all.equal}}}
\examples{
  v <- seq(0.7, 0.8, by=0.1)
  v[2]==0.8
  eq(v[2], 0.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
