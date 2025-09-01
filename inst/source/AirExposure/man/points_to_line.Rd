\name{points_to_line}
\alias{points_to_line}
\title{
Points to line
}
\description{
The purpose is to connect spatial points obtained with the request to the Tom-Tom API using lines on the street routes. Although it is an internal function, it can be used by the user considering the necessary arguments for this purpose
}
\usage{
points_to_line(data, long, lat, id_field = NULL, sort_field = NULL)
}
\arguments{
  \item{data}{
DataFrame that contains the coordinates of the points to be joined with lines. It must include information fields about latitude and longitude coordinates in decimal degree (EPSG:4326), line identification field (ID) and an additional column with point ordering values. The DataFrame used in this case corresponds to the output of the alternative_trajectories function, which must be modified to include the mentioned columns. 
}
  \item{long}{
String vector with the name of the field of the dataframe (data) that contains the longitude data. The values of the field should be in decimal degree (EPSG:4326).}
  \item{lat}{
String vector with the name of the field of the dataframe (data) that contains the latitude data. The values of the field should be in decimal degree (EPSG:4326).}
  \item{id_field}{
String with the name of the field containing the line identification field (e.g. "ID"). The points with the same ID belong to the same line/street. ID values can be numeric or string values. Can receive NULL values.}
  \item{sort_field}{
String vector with the name of the field with an identifier to order the points from smallest to largest according to their spatial location. The values must be numeric to represent the order sequence in which the points will be joined. This column is different from the "field_id" column}
}

\value{
Returns an object of type "sfc_LINESTRING" and "sfc" of the sf library, which can be visualized with the (plot) function.
}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\keyword{manip}
\concept{air}
\concept{pollution}
