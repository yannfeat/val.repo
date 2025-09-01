\name{CAPE_CIN}
\alias{CAPE_CIN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Calculation of CAPE and CIN
}
\description{
Taking into account the data obtained in a radiosonde, and after defining the initial values of the parcel, this function calculates the values of CAPE and CIN for the sounding.
}
\usage{
CAPE_CIN(Ps, Ts, ws, deltaP = 5, P0 = NA, T0 = NA, w0 = NA, PlowTop = NA, 
precoolType = "none", doLog = 0, getLiftedBack = FALSE, upToTop = TRUE, 
checkBuoyancy = 0)
}
\arguments{
  \item{Ps}{
Pressures (Pa) defining the sounding.
}
  \item{Ts}{
Temperatures (K) defining the sounding.
}
  \item{ws}{
Mixing ratios (kg/kg) defining the sounding.
}
  \item{deltaP}{
The width (Pa) of the layers used in the calculation of the numerical solution for the vertical evolution. A default value of 5 Pa is used. It must be positive.
}
  \item{P0}{
The initial pressure (Pa) for the parcel that is lifted (may be the lowest level of the sounding). Missing value is used by default.
}
  \item{T0}{
The initial temperature (K) of the parcel being lifted. Missing value is used by default.
}
  \item{w0}{
The initial mixing ratio (kg/kg) of the parcel being lifted.
}
  \item{PlowTop}{
If some layers must be averaged in the bottom of the sounding this argument provides the pressure (Pa) at the top of the layer that must be averaged in the bottom of the sounding. \code{NA} is used by default.
}
  \item{precoolType}{
If requested, an adiabatic or an isobaric precooling of the initial parcel is performed. "none" is used by default, but "adiabatic" and "isobaric" are also accepted.
}
  \item{doLog}{
Use logarithmic vertical interpolation between sounding levels if doLog=1. The default value is doLog=0.
}
  \item{getLiftedBack}{
TRUE/FALSE requests that the evolution of the lifted particle until the top level of the soundig is returned as a set of vectors for P, T and w (fields Pl, Tl and wl respectively). FALSE is used by default.
}
  \item{upToTop}{
TRUE(FALSE) requests that the lifted particle continues(stops) after the first crossing with the ambient sounding (EL) (until the sounding finishes). If TRUE, remaining negative areas above are accumulated into CIN only if the parcel becomes buoyant again in upper levels depending on the setting of \emph{checkBuoyancy}. TRUE is used by default.
}
  \item{checkBuoyancy}{
 If \emph{checkBuoyancy} is TRUE, the computation of CAPE and CIN proceed to the top of the sounding if \emph{upToTop} is TRUE if CAPE is larger than CIN while the parcel passes non-buoyant regions. The default value is FALSE.
}
}
\details{
CAPE and CIN (J/kg) are calculated from a sounding given by 1D arrays for pressure Ps (Pa), for temperature Ts (K) and for mixing ratio ws (kg/kg).

 If \eqn{P_0}/\eqn{T_0}/\eqn{w_0} are provided, no low vertical averaging is done and these values are used as initial points for the parcel. Missing value is used by default for these arguments.

This function returns some error codes in field \emph{outCode} in the return value if the computation of CAPE and CIN failed.
}
\value{

Returns:

\item{airStart}{
The real starting variable of the air parcel. It is a vector with 6 elements: P (Pa), Temp (K), w (kg/kg), theta (K), Tvirtual (K) and wsat (kg/kg). The values are computed depending on the input arguments.
}
\item{cape}{
CAPE index (J/kg).
}
\item{cin}{
CIN index (J/kg) as a negative number.
}
\item{apLCL}{
Variables of the air parcel at the Lifting Condensation Level (LCL). It is returned as a vector with 6 elements: P (Pa), Temp (K), w (kg/kg), theta (K), virtualT (K) and wsat (kg/kg).
}
\item{apLFC}{
Variables of the Level of Free Convection (LFC). If LFC is found, it is returned as a vector with six elements: P (Pa), Temp (K), w (kg/kg), theta (K), virtualT (K) and wsat (kg/kg).
}
\item{apEL}{
End Level (EL). If EL is found, it is returned as a vector with six elements: P (Pa), Temp (K), w (kg/kg), theta (K), virtualT (K) and wsat (kg/kg).
}
\item{gotLCL}{
TRUE/FALSE whether the LCL has been found or not.
}
\item{gotLFC}{
TRUE/FALSE whether the LFC has been found or not.
}
\item{gotEL}{
TRUE/FALSE whether the EL has been found or not.
}
\item{Pl}{
Pressure (Pa) at every step of the lifted particle during its evolution. If requested by using getLiftedBack==TRUE, every step until the end of the radiosonde is returned.
}
\item{Tl}{
Temp (K) at every step of the lifted particle during its evolution. If requested by using getLiftedBack==TRUE, every step until the end of the radiosonde is returned.
}
\item{wl}{
Mixing-ratio of the lifted particle during its evolution. If requested by using getLiftedBack==TRUE, every step until the end of the radiosonde is returned.
}
\item{Olifted}{
Number of elements in  Pl/Tl/wl.
}
\item{upToTop}{
Process the whole sounding even after finding the first "EL level".
}
\item{outCode}{
The  error code returned by the C routine that computes CAPE/CIN. If 0, everything has been OK!
}
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
capeCin<-CAPE_CIN(PlowTop=98000,precoolType="adiabatic",
                  Ps=aPs,Ts=aTs,ws=aws,doLog=0,deltaP=5,
                  getLiftedBack=TRUE,upToTop=TRUE)
print(min(capeCin$Tl))

#pdf("stuve.pdf")
stuveA<-stuve_diagram(Pres = aPs/100,Temp=aTs-273.15)
lines(capeCin$Tl-273.15,capeCin$Pl/100,col="red",lwd=2)
#dev.off()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}
