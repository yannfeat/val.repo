\name{trajectories_tomtom}
\alias{trajectories_tomtom}
\title{
Trajectories TomTom
}
\description{
Requests information from the Tom-Tom API related to traffic.
}
\usage{
trajectories_tomtom(origin, dest, mode, hour_trajectory, key)
}

\arguments{
\item{origin}{String vector with the geographical coordinates of the trip's origin in decimal degree (EPSG:4326).The point is considered as the person's home. It should follow the structure 'latitude,longitude'. Please avoid inserting spaces between the data to ensure the correct format.}
\item{dest}{
String vector with the geographical coordinates of the trip's destination in decimal degree (EPSG:4326). It should follow the structure 'latitude,longitude'. It corresponds to the place where the person is doing an activity. Please avoid inserting spaces between the data to ensure the correct format.
}
\item{mode}{ String vector which details the type of transport selected to make the trip from the origin to the destination point. The types of mobility will depend on the information provided by the TomTom API for the study site. The most common alternatives are: "car", "motorcycle", "pedestrian", "bicycle". For more information visit: https://developer.tomtom.com/routing-api/documentation/routing/calculate-route.}
\item{hour_trajectory}{String vector with the date and time when the journey took place. It must be entered in text string format with the following structure: 'YYYY-MM-DD HH:MM:SS}
\item{key}{String vector with your personal TomTom API password. This API is free of charge and allows up to 2500 requests per day but accessing the information requires a user account. For more information, visit: https://developer.tomtom.com/how-to-get-tomtom-api-key}
}
\details{
Seeks to obtain traffic information from the Tom-Tom API, by requesting six different travel alternatives based on the day, time, mode of transport, and the origin and destination points provided by the user.
}
\value{
Returns a dataframe with 14 fields, where: 1) ID corresponds to an identification number of each of the 6 alternatives provided by the API;  2) long and 3) lat correspond to the latitude and longitude of all the points along the route taken; 4) departureTime departure time from the origin to destination; 5) arrivalTime arrival time at the destination; 6) "alternative" name of the alternatives, from alternative_1 to alternative_6. he next fields contain information about the travel. Please see de documentation of Tom-Tom API of: "trafficLengthInMeters", "travelMode", "trafficDelayInSeconds",] "travelTimeInSeconds", "liveTrafficIncidentsTravelTimeInSeconds", "historicTrafficTravelTimeInSeconds", "noTrafficTravelTimeInSeconds"
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\note{An Internet connection is required to make requests to the TomTom API.}

\keyword{manip}
\concept{air}
\concept{pollution}
