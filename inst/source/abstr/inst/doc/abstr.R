## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(abstr)

## ----input, fig.cap="Example data that can be used as an input by functions in abstr to generate trip-level scenarios that can be imported by A/B Street."----
library(abstr)
library(tmap) # for map making
tm_shape(montlake_zones) + tm_polygons(col = "grey") +
  tm_shape(montlake_buildings) + tm_polygons(col = "blue")  +
tm_style("classic")

## ----output-sf, message=FALSE, warning=FALSE----------------------------------
head(montlake_od)

## ----message=FALSE,warning=FALSE----------------------------------------------
set.seed(42)
montlake_od_minimal = subset(montlake_od, o_id == "373" |o_id == "402" | o_id == "281" | o_id == "588" | o_id == "301" | o_id == "314")
output_sf = ab_scenario(
  od = montlake_od_minimal,
  zones = montlake_zones,
  zones_d = NULL,
  origin_buildings = montlake_buildings,
  destination_buildings = montlake_buildings,
  pop_var = 3,
  time_fun = ab_time_normal,
  output = "sf",
  modes = c("Walk", "Bike", "Drive", "Transit")
)

## ----outputplot---------------------------------------------------------------
tm_shape(output_sf) + tmap::tm_lines(col = "mode", lwd = .8, lwd.legeld.col = "black") +
  tm_shape(montlake_zones) + tmap::tm_borders(lwd = 1.2, col = "gray") +
  tm_text("id", size = 0.6) +
tm_style("cobalt")

## ----message=FALSE, warning=FALSE---------------------------------------------
output_json = ab_json(output_sf, time_fun = ab_time_normal, scenario_name = "Montlake Example")
ab_save(output_json, f = "montlake_scenarios.json")

## ---- include=FALSE-----------------------------------------------------------
# remove just generated .json file
file.remove("montlake_scenarios.json")

