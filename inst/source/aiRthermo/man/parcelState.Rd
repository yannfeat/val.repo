\name{parcelState}
\alias{parcelState}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
State of a parcel 
}
\description{
The function calculates the state of a parcel for easier computations.
}
\usage{
parcelState(Press, Temp, w = 0, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Press}{
Value of pressure (Pa) of the parcel.
}
  \item{Temp}{
Value of temperature (K) of the parcel.
}
  \item{w}{
Value of mixing ratio (kg/kg) of the parcel.
}
  \item{consts}{
  The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
	This function returns a list including the following values:
	\item{pressure}{ 
		Pressure value (Pa).
	}
	\item{temperature}{ 
		Temperature value (K).
	}
	\item{mixingratio}{ 
		Mixing ratio value (kg/kg).
	}
	\item{theta}{ 
		Potential temperature value (K).
	}
	\item{virtualTemp}{ 
		Virtual temperature value (K).
	}
	\item{saturationMixingRatio}{ 
		Saturation mixing ratio value (kg/kg).
	}
}
\seealso{
\code{\link{PT2Theta}}, \code{\link{virtual_temperature}} and
\code{\link{saturation_mixing_ratio}}
}
\examples{
parcelState(101325,273.15,0.2)
}
\keyword{Functions}