\name{stuve_diagram}
\alias{stuve_diagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thermodynamic (STUVE) Diagram
}
\description{
This function generates an Stüve diagram.

}
\usage{
stuve_diagram(Pres, Temp, TempD = NA, XLIM = c(-80, 45), YLIM = c(1050, 100), 
col.lines = NULL, lty.lines = NULL, lwd.lines = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Pres}{
A vector with pressure values in hPa.
}
  \item{Temp}{
A vector with temperature values in Celsius .
}
  \item{TempD}{
An optional vector with dew point temperatures in Celsius. The default value is NA.
}
  \item{XLIM}{
X axis limit in Celsius. Default value is c(-80, 45).
}
  \item{YLIM}{
Y axis limit in hPa. Default value is c(1050, 100).
}

\item{col.lines}{
A vector of colours for the stuve_diagram lines. They must be provided in this order: isotherms, isobars, dry adiabats, moist adiabats, constant mixing ratio lines and sounding. Default colours are c("grey", "grey", "olivedrab", "olivedrab", "brown", "red"). 
}

\item{lty.lines}{
A vector of line-types for the stuve_diagram. They must be provided following the same order as for the col.lines argument. Default values are c("dotted", "dotted", "dotted", "solid", "dotted", "solid").
}

\item{lwd.lines}{
A vector of line-widths for the stuve_diagram. They must be provided following the same order as for the col.lines and lty.lines arguments. Default values are c(2,2,2,1,2,1).
}

}
\details{
It is possible to add extra lines and to save as a pdf, jpeg or png (see examples).
}
\value{
The result is a plot object.
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
capeCin<-CAPE_CIN(PlowTop=98000,precoolType="adiabatic",
                  Ps=aPs,Ts=aTs,ws=aws,doLog=0,deltaP=5,
                  getLiftedBack=TRUE,upToTop=TRUE)

#How to add a line to the plot
stuveA<-stuve_diagram(Pres = aPs/100,Temp=aTs-273.15)
lines(capeCin$Tl-273.15,capeCin$Pl/100,col="blue",lwd=2)
}
\keyword{Functions}
