\name{total_exposure}
\alias{total_exposure}
\title{
Total exposure
}
\description{
Dynamic exposure model to atmospheric pollutants with dynamic variables
}
\usage{
total_exposure(travel_list, mode, dir, key, selection, output_exp, departure_time_home, 
               activity_minutes, pollutant, shapeValue, gridID, units)
}
\arguments{
  \item{travel_list}{
Dataframe with the coordinates (latitude and longitude) in decimal degree (EPSG:4326) of the origin(home) and destination(s)  
}
  \item{mode}{
String indicating the mode of transport used for each of the trips made to the destination(s). Last, the type of transportation back home must be included. This information depends on what is provided by the Tom-Tom API, specifically for the site of interest. The most common alternatives are: "bus", "car", "motorcycle", "pedestrian", "bicycle". The different alternatives can be combined. For more information, please visit https://developer.tomtom.com/.
}
  \item{dir}{
String vector with the path to the local directory where the shape files with the hourly pollutant grids are located.
}
  \item{key}{
String vector with your personal TomTom API password. This API is free of charge and allows up to 2500 requests per day but accessing the information requires a user account. For more information, visit: https://developer.tomtom.com/how-to-get-tomtom-api-key.
}
  \item{selection}{
String vector where the type of route to be used in each of the routes is indicated. The alternatives of this parameter can be: "faster" route, "shorter" route, route with lower concentrations as "lesspol", route with higher concentrations as "morepol", route with lower exposure as "lessexpos", route with higher exposure as "moreexpos" or a combination of them. You must enter the same number of data as the number of coordinates, since the return trip home is considered.
}
  \item{output_exp}{
String vector with the selected output format. It can be: a) "plot", which show the selected routes on an interactive map through the Leaflet library with information about de daily exposure estimated; b) "df" returns a dataframe with information on time, concentration and exposure in the 3 moments, during stay in origin, commuting and destination.
}
  \item{departure_time_home}{
String vector that provides the date-time of leaving home for the first time in the day to a destination. Must be in the format YYYYY-MM-DD HH:MM:SS to transform in an object POSIXct or POSIXt type.
}
  \item{activity_minutes}{
Dataframe with the time in minutes (number) of the duration of the activity(ies) performed at each destination.
}
  \item{pollutant}{
String vector with the name of the selected pollutant to be estimate. The model estimates exposures to the criteria pollutants according to the EPA's Air Quality Index (AQI). They must be entered in the following way: "PM10", "PM2.5", "SO2", "NO2", "CO", "O3". 
}
  \item{shapeValue}{
String vector with the name of the field corresponding to the concentrations in the shapefile files (e.g. "value").
}
  \item{gridID}{
String vector with the name of the field containing the unique identifier (ID) for the grid. The ID is a unique number assigned to each pixel within the concentration grid in the shapefile (e.g. "GRI1_ID")
}
  \item{units}{
String vector with the units of the concentrations of the pollutant. For example, PM2.5 and PM10 should be in ug/ m3, CO and O3 should be in ppm, SO2 and NO2 should be in ppb.
}
}
\details{
Estimates daily exposure by taking into account dynamic variables, specifically the mobility and activities of individuals throughout the day and in different locations. Accounts for the 3 exposure moments of the day, at the origin, during commuting, and at the destination. 
}
\value{
It returns the result of the daily exposure estimation of the pollutant of interest considering the routes and activities. The function can return two types of outputs
\item{Dataframe }{with the results of the estimation divided into each site visited and each route taken.}
\item{Interactive map}{with Leaflet showing the selected routes and the daily concentration grid.}
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\note{The function incorporates other functions like trajectories_tomtom, alternative_trajectories, points_to_line, temporary_grid_search, map_colors and hourly_grid.
}


\seealso{
function_hours, map_colors(), hourly_grid(),alternative_trajectories 
}

\keyword{manip}
\concept{air}
\concept{pollution}
