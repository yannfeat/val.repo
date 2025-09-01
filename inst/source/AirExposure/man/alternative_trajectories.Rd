\name{alternative_trajectories}
\alias{alternative_trajectories}
\title{
Alternative Trajectories
}
\description{
Visualization of the response obtained from the request made to the TomTom API. It shows six travel alternatives between the origin and destination points
}
\usage{
alternative_trajectories(origin, dest, mode, dir, key, output, hours = NULL, gridID, 
shapeValue, units, pollutant)
}
\arguments{
  \item{origin}{
String vector with the geographical coordinates of the trip's origin in decimal degree (EPSG:4326). This point is considered as the person's home. It should follow the structure 'latitude,longitude'. Please avoid inserting spaces between the data to ensure the correct format
}
  \item{dest}{
String vector with the geographical coordinates of the trip's destination in decimal degree (EPSG:4326). It should follow the structure 'latitude,longitude'. Please avoid inserting spaces between the data to ensure the correct format.
}
  \item{mode}{
String vector which details the type of transport selected to make the trip from the origin to the destination point. The types of mobility will depend on the information provided by the TomTom API for the study site. The most common alternatives are: 'car', 'motorcycle', 'pedestrian', 'bicycle'. For more information visit: https://developer.tomtom.com/routing-api/documentation/routing/calculate-route
}
  \item{dir}{
String vector with the path to the local directory where the shape files with the hourly pollutant grids are located.
}
  \item{key}{
String vector with your personal Tom-Tom API password. This API is free of charge and allows up to 2500 requests per day but accessing the information requires a user account. For more information, visit: https://developer.tomtom.com/how-to-get-tomtom-api-key.
}
  \item{output}{
String vector with the selected output format. It can be: a) plot, which show the alternative routes on an interactive map through the Leaflet library; b) df  returns a dataframe with information on time, distance, concentrations of the alternative routes; c) polyline, returns a sf/data.frame (a line) object, which can be exported in shapefile format with sf library. The selection must be in lower case.
}
  \item{hours}{
String vector with the date and time when the commuting takes place. It must be entered with the following structure 'YYYY-MM-DD HH:MM:SS' so that it is recognized as an object of type POSIXct or POSIXt within the model (e.g. 2019-08-01 08:00:00).
}
  \item{gridID}{
String vector with the name of the field containing the unique identifier (ID) for the grid. The ID is a unique number assigned to each pixel within the concentration grid in the shapefile (e.g. 'GRI1_ID')
}
  \item{shapeValue}{
String vector with the name of the field which correspond to the concentrations values in the shapefile (e.g. 'value')
}
  \item{units}{
String with the units of measurement of the pollutant concentrations used to estimate the exposure. For, PM2.5 and PM10 should be in 'ug/m3'; CO and O3 should be in 'ppm'; SO2 and NO2 should be in 'ppb'. The units of measurement as well as the pollutants are based on the Air Quality Index(AQI) determined by the US EPA.
}
  \item{pollutant}{
String  vector with the name of the selected pollutant. The model can estimate exposure for the following pollutants: PM10, PM2.5, SO2, NO2, CO, O3.
}
}
\details{
From the request for information on route alternatives returned by trajectories_tomtom, this function has the purpose  to classify and shows the routes obtained in three different formats. After obtaining the six route alternatives, the next step involves classifying the route types into: i) 'faster' (according to travel time - faster); ii) 'shorter' (according to travel distance - short); iii) more polluted and less polluted (according to the concentrations for the specified day and time - 'morepol' and 'lesspol'); iv) higher and lower exposure (according to the concentrations and the duration of each route - 'moreexpos' and 'lessexpos'). The routes can be the same for different alternatives. For instance, the most polluted route may coincide with the fastest route. The function is used by total_exposure to obtain each of the trips taken during the day by an individual.
}
\value{
Returns the specified object in the output parameter:
\item{df}{it provides information in a dataframe objset about the route. The variables include: alternative; ID, long, lat, departureTime, arrivalTime, lengthInMeters trafficLengthInMeters, travelMode, trafficDelayInSeconds, travelTimeInSeconds, liveTrafficIncidentsTravelTimeInSeconds, historicTrafficTravelTimeInSeconds, noTrafficTravelTimeInSeconds daily_pol_value_mean, exposure_value_mean and type}
\item{plot}{It returns an object of type leaflet and htmlwidget corresponding to an interactive map where it is possible to visualize the streets through which each of the alternatives will pass}
\item{polyline}{It returns an sf object with the same variables as df output but it can be represented as a geospatial object on a map, displaying spatial lines corresponding to each of the alternatives that traverse streets and routes.}
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\note{
For more information about the variables obtained from the traffic data request to the TomTom API, please review the documentation of the calculateRoute service. TomTom Routing API Documentation https://developer.tomtom.com/routing-api/documentation/product-information/introduction
}

\keyword{manip}
\concept{air}
\concept{pollution}

