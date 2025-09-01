\name{temporary_grid_search}
\alias{temporary_grid_search}
\title{
Temporary grid search
}
\description{
The aim is to search for the grid with the name obtained in the function time_grid in the local path entered. If only one hour is entered, it returns the grid in shapefile format saved in the local directory. In the case of a range of hours, an average grid of the time range is returned.}
\usage{
temporary_grid_search(start_hour, end_hour = NULL, dir, time_format, gridID, shapeValue)
}
\arguments{
  \item{start_hour}{
String vector that represents the date and time of interest to estimate the exposure, indicating the start time of the activity and/or the trip. 
}
  \item{end_hour}{
String vector that represents the date and time of interest to estimate the exposure, indicating the end of the activity and/or the trip. If you want to obtain a specific time interval that denotes the duration of an activity and/or trip of the same day, you need to provide this parameter. If not, it defaults to NULL.
}
  \item{dir}{
String vector with the path to the local directory where the shapefiles that have the information about the concentrations of the pollutants of interest are located.
}
  \item{time_format}{
String vector representing the structure using special strptime abbreviations, indicating how the date data was provided (e.g. %Y-%m-%d %H:%M:%S)
}
  \item{gridID}{
String vector with the name of the field containing the unique identifier (ID) for the grid. The ID is a unique number assigned to each pixel within the concentration grid in the shapefile (e.g. GRI1_ID)
}
  \item{shapeValue}{String vector with the name of the field corresponding to the concentrations in the shapefile files (e.g. value)}
}
\details{
The aim is to search for the grid with the name obtained in the function time_grid in the local path entered. If only one hour is entered, it returns the grid in shapefile format saved in the local directory. In the case of a range of hours, an average grid of the time range is returned.
}
\value{
This function is used internally in the model of dynamic and fixed variables. The output is the reading of the grid in local path, where a file of class "sf" and "data.frame" is read.
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\keyword{manip}
\concept{air}
\concept{pollution}
