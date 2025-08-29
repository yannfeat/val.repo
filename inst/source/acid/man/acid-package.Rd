\name{acid-package}
\alias{acid-package}
\alias{acid}
\docType{package}
\title{
Analysing Conditional Income Distributions
}
\description{Functions for the analysis of income distributions for subgroups of the population as defined by a set of variables like age, gender, region, etc. 
This entails a Kolmogorov-Smirnov test for a mixture distribution as well as functions for moments, inequality measures, entropy measures and polarisation measures of income distributions. 
This package thus aides the analysis of income inequality by offering tools for the exploratory analysis of income distributions at the disaggregated level. 
}
\details{
\tabular{ll}{
Package: \tab acid\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-01-06\cr
License: \tab GPL-3\cr
}

\code{\link{sadr.test}}, 
\code{\link{polarisation.ER}},
\code{\link{gini.den}}  
}

\references{
Klein, N. and Kneib, T., Lang, S. and Sohn, A. (2015): 
Bayesian Structured Additive Distributional Regression with an Application to Regional Income Inequality in Germany, in: 
Annals of Applied Statistics, Vol. 9(2), pp. 1024-1052.

Sohn, A., Klein, N. and Kneib. T. (2014): A New Semiparametric Approach to Analysing Conditional Income Distributions, in: SOEPpapers, No. 676.
%% ~put references to the literature/web site here ~
}



\author{
Alexander Sohn <asohn@uni-goettingen.de>
}

\keyword{ package }
\seealso{ \code{\link[gamlss]{gamlss}}, \code{\link[ineq]{ineq}}
}
