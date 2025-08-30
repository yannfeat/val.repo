\name{parconv}
\alias{parconv}
\title{Function to convert between two different parameterizations of a family of conditional error functions}
\description{
This function converts between two different parameterizations of a family of conditional error functions: a (more \sQuote{traditional}) parameter \eqn{c}, and a (more convenient) parameter \eqn{\alpha_2}{alpha2} specifying the local level of the test after the second stage.
}
\usage{
parconv(typ, a2 = NA, c = NA)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{a2}{\eqn{\alpha_2}{alpha2}, the local level of the test after the second stage (see details)}
  \item{c}{the parameter \eqn{c} (see details)}
}
\details{
   Traditionally, a family of conditional error functions is often parameterized by some parameter \eqn{c} that, in turn, depends on the local level \eqn{\alpha_2}{alpha2} of the test after the second stage. However, it can be convenient to parameterize the family directly by \eqn{\alpha_2}{alpha2}. The function \code{parconv} converts one parameter into the other: provide one, and it returns the other.

   Essentially, the relation between the two parameterizations is implemented as:
   \itemize{
     \item \eqn{c = \exp(-\chi^2_{4,\alpha_2}/2)}{c = exp(-chi2_{4,alpha2}/2)} for Fisher's combination test (Bauer and Koehne, 1994)
     \item \eqn{c = \Phi^{-1}(1-\alpha_2)}{c = Phi^{-1}(1-alpha2)} for the inverse normal method (Lehmacher and Wassmer, 1999)
     \item \eqn{\alpha_2 = {(\Gamma(1+1/r))^2}/{\Gamma(1+2/r)}}{alpha2 = {(Gamma(1+1/r))^2}/{Gamma(1+2/r)}} for Vandemeulebroecke (2006)
     \item \eqn{c = \alpha_2}{c = alpha2} for the family of horizontal conditional error functions
   }
}
\value{
  \code{parconv} returns \eqn{\alpha_2}{alpha2} corresponding to the supplied \eqn{c}, or \eqn{c} corresponding to the supplied \eqn{\alpha_2}{alpha2}.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.
}
\author{Marc Vandemeulebroecke}
\note{
  Provide either \code{a2} or \code{c}, not both!

  \eqn{\alpha_2}{alpha2} is the local level of the test after the second stage, and it equals the integral under the corresponding conditional error function: \deqn{\alpha_2 = \int_0^1 cef_{\alpha_2}(p_1) d p_1,}{alpha2 = int_0^1 cef_{alpha2}(p1) d p1,} where \eqn{cef_{\alpha_2}}{cef_{alpha2}} is the conditional error function (of a specified family) with parameter \eqn{\alpha_2}{alpha2}.

  Note that in this implementation of adaptive two-stage tests, early stopping bounds are \emph{not} part of the conditional error function. Rather, they are specified separately (see also \code{\link{tsT}}).

  \eqn{\alpha_2}{alpha2} can take any value in \eqn{[0,1]}; \eqn{c} can take values in
    \itemize{
      \item \eqn{[0,1]} for Fisher's combination test (Bauer and Koehne, 1994)
      \item \eqn{(-\infty, \infty)}{(-infty, infty)} for the inverse normal method (Lehmacher and Wassmer, 1999)
      \item \eqn{[0,\infty)}{[0,infty)} for Vandemeulebroecke (2006)
      \item \eqn{[0,1]} for the family of horizontal conditional error functions
    }
}
\seealso{\code{\link{adaptTest}} package description, \code{\link{getpar}}, \code{\link{CEF}}}
\examples{
## Obtain the parameter c for Fisher's combination test, using
##  the local level 0.05 for the test after the second stage
parconv(typ="b", a2=0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
