\name{hourly_grid}
\alias{hourly_grid}
\title{
hourly_grid
}
\description{
Internal model function to search and identify the name of the concentration hourly grid associated with the date-time of the input data.
}
\usage{
hourly_grid(hour_input, time_format, dir)
}

\arguments{
  \item{hour_input}{
String vector with the date-time to determine the concentrations grid of interest. It is recommended to input with the following structure: "YYYY-MM-DD HH:MM:SS" to transform in an object POSIXct or POSIXt type.
}
 \item{time_format}{
String vector representing the structure using special strptime abbreviations, indicating how the date data was provided (e.g. Y-m-d H:M:S).
}

  \item{dir}{
String with the local directory path where the shapefiles containing information about concentrations of the pollutants of interest are located.
}
}
\details{
The purpose of the function is to navigate through the user-provided directory containing the shapefiles with concentration grids to search and identify the name of the concentration hourly grid. The information is related to the date on which the daily air exposure is to be estimated. This is an internal function of the airExposure model, but is available if the user wishes to use it to search and check their files.
}
\value{
Returns a string vector with the name of the file matching the input variables (date-time) which is the date of interest for estimating air exposure 
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\note{
The shapefile files located in the local path should be 24 per day, that is, one grid per hour per day. Each shapefile should be named as YYYY-MM-DD_HHMM (e.g. 2019-08-01_1600). In the case of the 00:00 hour, must be set in a different way, with a 1  value in the the last value of minutes: YYYY-MM-DD_HHM1 (e.g. 2019-08-01_0001). All files must have at least two fields, such as: grid identification (e.g., ID) and pollutant concentration value (e.g., value). 
}

\keyword{manip}
\concept{air}
\concept{pollution}