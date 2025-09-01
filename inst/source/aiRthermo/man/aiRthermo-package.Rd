\name{aiRthermo-package}
\alias{aiRthermo-package}
\alias{aiRthermo}
\docType{package}
\title{
Atmospheric Thermodynamics and Visualization
}
\description{
Deals with many computations related to the thermodynamics of atmospheric processes. It includes many functions designed to consider the density of air with varying degrees of water vapour in it, saturation pressures and mixing ratios, conversion of moisture indices, computation of atmospheric states of parcels subject to dry or pseudoadiabatic vertical evolutions and atmospheric instability indices that are routinely used for operational weather forecasts or meteorological diagnostics.

Unless otherwise explicitly noted (\code{\link{boltonTLCL}} and \code{\link{stuve_diagram}}) all parameters to functions must be provided in the International System of Units: P in Pa, T in K and w in kg/kg.
}

\author{
Jon Sáenz, Santos J. González-Rojí, Sheila Carreno-Madinabeitia and Gabriel Ibarra-Berastegi

Maintainer: Santos J. González-Rojí <santosjose.gonzalez@ehu.eus>
}


\examples{

# CAPE, CIN index
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
capeCin<-CAPE_CIN(PlowTop=98000,precoolType="adiabatic",
                  Ps=aPs,Ts=aTs,ws=aws,doLog=0,deltaP=5,
                  getLiftedBack=TRUE,upToTop=TRUE)
print(min(capeCin$CAPE))


#pdf("stuve.pdf")
stuveA<-stuve_diagram(Pres = aPs/100,Temp=aTs-273.15)
lines(capeCin$Tl-273.15,capeCin$Pl/100,col="red",lwd=2)
#dev.off()

# Adiabatic Ascent
P0<-101325
T0<-273.15
w0<-0.0025
adiabEvol<-adiabatic_ascent(P0,T0,w0,50000,5)

}
\keyword{ Package }
