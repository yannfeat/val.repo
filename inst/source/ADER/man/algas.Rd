\encoding{latin1}
\name{algas}
\alias{algas}
\docType{data}
\title{
Size and Reproduction in Codium Seaweeds
}
\description{
Data about size and reproductive status in randomly collected individuals  of three species of \emph{Codium} seaweeds in the Cantabrian coast.
}
\usage{data("algas")}
\format{
  A data frame with 359 observations on the following 3 variables.
  \describe{
    \item{\code{Sp}}{Species of \emph{Codium}. A factor with levels \code{Fra} (\emph{C. fragile}), \code{Tom} (\emph{C. tomentosum}) and \code{Ver}(\emph{C. vermilara}).}
    \item{\code{Long}}{Length of each individual.}
    \item{\code{Estado}}{Presence (1) or absence (0) of reproductive structures. }
  }
}

\source{
\enc{Rojo, I., Olabarria, C., Santamaria, M., Provan, J., Gallardo, T. and Viejo, R.M.
2014. Coexistence of congeneric native and invasive species: The case of
the green algae \emph{Codium} spp. in northwestern Spain. Marine
\emph{Environmental Research} 101: 135-144.}{Rojo, I., Olabarria, C., Santamaria, M., Provan, J., Gallardo, T. and Viejo, R.M.
2014. Coexistence of congeneric native and invasive species: The case of
the green algae \emph{Codium} spp. in northwestern Spain. Marine
\emph{Environmental Research} 101: 135-144.}
}
\examples{
data(algas)
str(algas)
}
\keyword{datasets}
