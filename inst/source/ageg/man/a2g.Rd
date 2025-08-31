\name{a2g}
\alias{a2g}
\title{Age to Group}
\arguments{
\item{ages}{Vector of Numeric class age values.}
\item{mydist}{Vector of Character class age distributions.  Values must be two integers separated by a hyphen.  Remove any whitespaces on either side of the hyphen.}
}

\usage{
a2g(ages, mydist)
}
\description{
Function that converts numeric age values to user defined age groups.
}

\value{
Returns a Character class vector object of age distribution values defined by the mydist argument.  The function will otherwise return an error message stating that the function requires a numeric class object.
}

\examples{
ages <- c(3, 101, 42, 32)
mydist  <- c("5-10","11-20","21-30","31-40","41-50","51-60","61-70","71+")

a2g(ages, mydist)

# > a2g(ages, mydist)
# [1] NA      "71+"   "41-50" "31-40"

}



