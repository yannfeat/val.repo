\name{d2a}
\alias{d2a}
\title{Date to Age}
\arguments{
\item{bd}{Vector of Date class values.  The date "yyyy-mm-dd" format is expected. }


}

\usage{
d2a(bd)
}
\description{
Function that converts a Date class vector of birth date values to numeric age values. This function is preparative to the a2g function.
}

\value{
Returns a numeric class vector object of ages if the function is provided a Date class object.  The function will otherwise return an error message stating that the function requires a Date class object.
}

\examples{
datevals <- as.Date(c("2019-01-01","1920-12-12","1980-02-02","1991-03-03" ))


d2a(datevals)

# > d2a(datevals)
# [1]  3 101  42  31

}


