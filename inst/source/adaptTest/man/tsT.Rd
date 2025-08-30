\name{tsT}
\alias{tsT}
\title{Function to implement an adaptive two-stage test}
\description{
  There are four key quantities for the specification of an adaptive two-stage test: the overall test level \eqn{\alpha}{alpha}, stopping bounds \eqn{\alpha_1 <= \alpha_0}{alpha1 <= alpha0} and the local level \eqn{\alpha_2}{alpha2} of the test after the second stage. These quantities are interrelated through the overall level condition. The function \code{tsT} calculates any of these quantities based on the others.
}
\usage{
tsT(typ, a = NA, a0 = NA, a1 = NA, a2 = NA)
}
\arguments{
  \item{typ}{type of test: \code{"b"} for Bauer and Koehne (1994), \code{"l"} for Lehmacher and Wassmer (1999), \code{"v"} for Vandemeulebroecke (2006) and \code{"h"} for the horizontal conditional error function}
  \item{a}{\eqn{\alpha}{alpha}, the overall test level}
  \item{a0}{\eqn{\alpha_0}{alpha0}, the futility stopping bound}
  \item{a1}{\eqn{\alpha_1}{alpha1}, the efficacy stopping bound and local level of the test after the first stage}
  \item{a2}{\eqn{\alpha_2}{alpha2}, the local level of the test after the second stage}
}
\details{
An adaptive two-stage test can be viewed as a family of decreasing functions \eqn{f[c](p_1)}{f[c](p1)} in the unit square. Each of these functions is a conditional error function, specifying the type I error conditional on the p-value \eqn{p_1}{p1} of the first stage. For example, \eqn{f[c](p_1) = \min(1, c/p_1)}{f[c](p1) = min(1, c/p1)} corresponds to Fisher's combination test (Bauer and Koehne, 1994). Based on this function family, the test can be put into practice by specifying the desired overall level \eqn{\alpha}{alpha}, stopping bounds \eqn{\alpha_1 <= \alpha_0}{alpha1 <= alpha0} and a parameter \eqn{\alpha_2}{alpha2}. After computing \eqn{p_1}{p1}, the test stops with or without rejection of the null hypothesis if \eqn{p_1 <= \alpha_1}{p1 <= alpha1} or \eqn{p_1 > \alpha_0}{p1 > alpha0}, respectively. Otherwise, the null hypothesis is rejected if and only if \eqn{p_2 <= f[c](p_1)}{p2 <= f[c](p1)} holds for the p-value \eqn{p_2}{p2} of the second stage, where \eqn{c} is such that the local level of this latter test is \eqn{\alpha_2}{alpha2} (e.g., \eqn{c = c(\alpha_2) = \exp(-\chi^2_{4,\alpha_2}/2)}{c = c(alpha2) = exp(-chi2_{4,alpha2}/2)} for Fisher's combination test).

The four parameters \eqn{\alpha}{alpha}, \eqn{\alpha_0}{alpha0}, \eqn{\alpha_1}{alpha1} and \eqn{\alpha_2}{alpha2} are interdependent: they must satisfy the level condition \deqn{\alpha_1 + \int_{\alpha_1}^{\alpha_0} cef_{\alpha_2}(p_1) d p_1 = \alpha,}{alpha1 + int_{alpha1}^{alpha0} cef_{alpha2}(p1) d p1 = alpha,} where \eqn{cef_{\alpha_2}}{cef_{alpha2}} is the conditional error function (of a specified family) with parameter \eqn{\alpha_2}{alpha2}. For example, this conditon translates to \deqn{\alpha = \alpha_1 + c(\alpha_2) * (\log(\alpha_0) - \log(\alpha_1))}{alpha = alpha1 + c(alpha2) * (log(alpha0) - log(alpha1))} for Fisher's combination test (assuming that \eqn{c(\alpha_2) < \alpha_1}{c(alpha2) < alpha1}; Bauer and Koehne, 1994). The function \code{tsT} calculates any of the four parameters based on the remaining ones. Currently, this is implemented for the following four tests: Bauer and Koehne (1994), Lehmacher and Wassmer (1999), Vandemeulebroecke (2006), and the horizontal conditional error function.
}
\value{
If three of the four quantities \eqn{\alpha}{alpha}, \eqn{\alpha_0}{alpha0}, \eqn{\alpha_1}{alpha1} and \eqn{\alpha_2}{alpha2} are provided, \code{tsT} returns the fourth. If only \eqn{\alpha}{alpha} and \eqn{\alpha_0}{alpha0} are provided, \code{tsT} returns \eqn{\alpha_1}{alpha1} under the condition \eqn{\alpha_1 = \alpha_2}{alpha1 = alpha2} (the so-called "Pocock-type").

If the choice of arguments is not allowed (e.g., \eqn{\alpha_0 < \alpha_1}{alpha0 < alpha1}) or when a test cannot be constructed with this choice of arguments (e.g., \eqn{\alpha_0 = 1}{alpha0 = 1} and \eqn{\alpha < \alpha_2}{alpha < alpha2}), \code{tsT} returns \code{NA}.

IMPORTANT: When the result is (theoretically) not unique, \code{tsT} returns the maximal \eqn{\alpha_1}{alpha1}, maximal \eqn{\alpha_2}{alpha2} or minimal \eqn{\alpha_0}{alpha0}.

In all cases, \code{tsT} returns the result for the test specified by \code{typ}.
}
\references{
  Bauer, P., Koehne, K. (1994). Evaluation of experiments with adaptive interim analyses. \emph{Biometrics} 50, 1029-1041.

  Lehmacher, W., Wassmer, G. (1999). Adaptive sample size calculations in group sequential trials. \emph{Biometrics} 55, 1286-1290.

  Vandemeulebroecke, M. (2006). An investigation of two-stage tests. \emph{Statistica Sinica} 16, 933-951.

  Vandemeulebroecke, M. (2008). Group sequential and adaptive designs - a review of basic concepts and points of discussion. \emph{Biometrical Journal} 50, 541-557.
}
\author{Marc Vandemeulebroecke}
\note{
The argument \code{typ}, and either exactly three of \eqn{\alpha}{alpha}, \eqn{\alpha_0}{alpha0}, \eqn{\alpha_1}{alpha1} and \eqn{\alpha_2}{alpha2}, or only \eqn{\alpha}{alpha} and \eqn{\alpha_0}{alpha0}, must be provided to \code{tsT}.
}
\seealso{\code{\link{adaptTest}} package description}
\examples{
## Example from Bauer and Koehne (1994): full level after final stage, alpha0 = 0.5
alpha  <- 0.1
alpha2 <- 0.1
alpha0 <- 0.5
alpha1 <- tsT(typ="b", a=alpha, a0=alpha0, a2=alpha2)
plotCEF(typ="b", a2=alpha2, add=FALSE)
plotBounds(alpha1, alpha0)

## See how similar Lehmacher and Wassmer (1999) and Vandemeulebroecke (2006) are
alpha  <- 0.1
alpha1 <- 0.05
alpha0 <- 0.5
alpha2l <- tsT(typ="l", a=alpha, a0=alpha0, a1=alpha1)
alpha2v <- tsT(typ="v", a=alpha, a0=alpha0, a1=alpha1)
plotCEF(typ="l", a2=alpha2l, add=FALSE)
plotCEF(typ="v", a2=alpha2v, col="red")
plotBounds(alpha1, alpha0)

## A remark about numerics
tsT(typ="b", a=0.1, a1=0.05, a0=0.5)
tsT(typ="b", a=0.1, a2=0.104877, a0=0.5)
tsT(typ="b", a=0.1, a2=tsT(typ="b", a=0.1, a1=0.05, a0=0.5), a0=0.5)

## An example of non-uniqueness: the maximal alpha1 is returned; any
##  smaller value would also be valid
alpha  <- 0.05
alpha0 <- 1
alpha2 <- 0.05
alpha1 <- tsT(typ="b", a=alpha, a0=alpha0, a2=alpha2)
tsT(typ="b", a0=alpha0, a1=alpha1, a2=alpha2)
tsT(typ="b", a0=alpha0, a1=alpha1/2, a2=alpha2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
