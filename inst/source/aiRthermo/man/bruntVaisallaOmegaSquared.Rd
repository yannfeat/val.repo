\name{bruntVaisallaOmegaSquared}
\alias{bruntVaisallaOmegaSquared}

\title{
Brunt-Vaisalla (angular) frequency (squared)
}
\description{
Brunt-Vaisalla (angular) frequency (aquared, \eqn{s^{-2}}) considering hydrostatic equilibrium. P is used as a vertical level. 
}
\usage{
bruntVaisallaOmegaSquared(Ps, Ts, ws, consts = export_constants())
}
\arguments{
  \item{Ps}{
    A vector with pressure values (Pa).
}
  \item{Ts}{
    A vector with temperature values (K).
}
  \item{ws}{
  A vector with mixing ratio values (kg/kg).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. The constants g and Rd are used.
}
}
\details{
The angular frequency (squared, \eqn{s^{-2}}) is returned in order to avoid complex numbers. 
}
\value{
The Brunt-Vaisalla (angular) frequency (squared) is returned.

}
\note{
For stable atmospheres, should be positive at every level. Ps, Ts and ws are 1D arrays.
}
\seealso{
\code{\link{PT2Theta}} and \code{\link{densityMoistAir}} are used inside \code{bruntVaisallaOmegaSquared} function. 
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
bruntVaisallaOmegaSquared(dPs,dTs,dws)

}
\keyword{Functions}