\name{map_colors}
\alias{map_colors}
\title{
Map colors.
}
\description{
Its purpose is to color the pollutant grids according to the EPA Air Quality Index (AQI). It corresponds to an internal function since it is used in the functions total_exposure, traditional_model and alternative_trajectories. It needs the pollutant grid and its name, adding two new columns with the color and the AQI breakpoints. The colors and their ranges will depend on the pollutant of interest.
}
\usage{
map_colors(grid, pollutant)
}
\arguments{
  \item{grid}{
Variable corresponding to the grid of interest in shapefile format.
}
  \item{pollutant}{
String  vector with the name of the selected pollutant. The model can estimate exposure for the following pollutants: PM10, PM2.5, SO2, NO2, CO, O3.}

}
\details{
Allows for the configuration of colors for pollutant grids based on the EPA's Air Quality Index (AQI) ranges, determining potential effects of exposure to specific atmospheric pollutants. Within the package are functions like total_exposure, traditional_model and alternative_trajectories, that facilitate visualizing outputs via an interactive leaflet map. The map_color function uses pollutant concentration grids and, depending on the chosen function mention, generates a new grid with additional fields where colors and ranges are set for potential pollutant effects. Subsequently, each of the mentioned functions takes these columns and returns an interactive map with colored grids. The categories could be string values like "Good", "Moderate","Unhealthy for sensible groups", "Unhealthy", "Very Unhealthy", "Hazardous". The field colors is a string with the HEX color code like "#abdda4", "#f8fd66", "#fdde61", "#d74a4c", "#b687ba","#590e632"
}
\value{Returns the input grid with two additional fields named categories and color which are utilized in the total_exposure, traditional_model and alternative_trajectories functions to visualize data in interactive Leaflet maps. }
\references{United States Environmental Protection Agency. (2018). Technical Assistance Document for the Reporting of Daily Air Quality - the Air Quality Index (AQI). Environmental Protection, 22. https://airnowtest.epa.gov/sites/default/files/2018-05/aqi-technical-assistance-document-may2016.pdf}

\author{
Josefina Urquiza, Maria Florencia Tames, Maela Lupo, Alfredo Rigalli

Maintainer: Josefina Urquiza <jurquiza@conicet.gov.ar>
}

\keyword{manip}
\concept{air}
\concept{pollution}