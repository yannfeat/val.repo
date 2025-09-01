\name{equivalentPotentialTemperature}
\alias{equivalentPotentialTemperature}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Equivalent Potential Temperature 
}
\description{
This function calculates the equivalent potential temperature (K), following the techniques used in Davies-Jones (2009).
}
\usage{
equivalentPotentialTemperature(P, Temp, w, TLCL, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
    The pressure (Pa) of the air parcel.
}
  \item{Temp}{
    The temperature (K) of the parcel. 
}
  \item{w}{
  The mixing ratio (kg/kg) of the parcel.
}
  \item{TLCL}{
  The temperature (K) at the Lifting Condensation Level (LCL).
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
This function returns the value of the equivalent potential temp (K).
}
\references{
Davies-Jones, R. (2009). On formulas for equivalent potential temperature. Monthly Weather Review, 137(9), 3137-3148.
}

\seealso{
 \code{\link{PT2Theta}} and \code{\link{moistCp}}
}
\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aP0<-aPs[1]
aT0<-C2K(RadiosondeA[1,3])
aw0<-RadiosondeA[1,6]/1000
deltaP=1
Na=length(aPs)
Ptop=aPs[Na]
fndlcl=find_lcl(Ptop,aP0,aT0,aw0,deltaP)
TLCL=fndlcl$Tlcl
equivalentPotentialTemperature(aP0,aT0,aw0,TLCL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Functions}