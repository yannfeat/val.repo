## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(abstr)

## ----getlocations, eval=FALSE, echo=FALSE-------------------------------------
#  home = stplanr::geo_code("potternewton park")
#  work = stplanr::geo_code("university of leeds")
#  park = stplanr::geo_code("woodhouse moore park")
#  cafe = stplanr::geo_code("heart centre leeds")
#  cafe = stplanr::geo_code("worsley building leeds")

## ----places-------------------------------------------------------------------
places = tibble::tribble(
  ~name, ~x, ~y,
  "Home", -1.524, 53.819,
  "Work", -1.552, 53.807,
  "Park", -1.560, 53.812,
  "Cafe", -1.556, 53.802
)
places_sf = sf::st_as_sf(places, coords = c("x", "y"), crs = 4326)
plot(places_sf, pch = places$name)
# mapview::mapview(places_sf, pch = places$name)

## ----placestoosimple, eval=FALSE, echo=FALSE----------------------------------
#  places = tibble::tribble(
#    ~name, ~x, ~y,
#    "Home", 5, 0,
#    "Work", 2, 5,
#    "Park", 1, 4,
#    "Cafe", 2, 4
#  )
#  places_sf = sf::st_as_sf(places, coords = c("x", "y"))
#  plot(places_sf, pch = places$name)

## ----lines--------------------------------------------------------------------
od = tibble::tribble(
  ~o, ~d, ~mode, ~departure, ~person,
  "Home", "Work", "Bike", "08:30", 1,
  "Work", "Park", "Walk", "11:30", 1,
  "Park", "Cafe", "Walk", "12:15", 1,
  "Cafe", "Work", "Walk", "12:45", 1,
  "Work", "Home", "Bike", "17:00", 1
)

## -----------------------------------------------------------------------------
od_sf = od::od_to_sf(od, places_sf)
plot(od_sf["departure"], reset = FALSE, key.pos = 1, lwd = 6:2)
plot(places_sf$geometry, pch = places$name, add = TRUE, cex =2)
# mapview::mapview(od_sf["departure"])

## -----------------------------------------------------------------------------
(od::od_coordinates(od_sf))

## -----------------------------------------------------------------------------
departure_times = c(
  8.5,
  11.5,
  12.25,
  12.75,
  17
)
set.seed(42) # if you want deterministic results, set a seed.
od_sf$departure = ab_time_normal(hr = departure_times, sd = 0.15, n = length(departure_times))

## -----------------------------------------------------------------------------
od_json1 = ab_json(od_sf[1, ], scenario_name = "activity")
od_json = ab_json(od_sf, scenario_name = "activity")

## ----absave-------------------------------------------------------------------
ab_save(od_json1, f = "scenario1.json")

## -----------------------------------------------------------------------------
# Save in the current directory:
ab_save(od_json, f = "activity_leeds.json")
# Save in a directory where you cloned the abstreet repo for the simulation
# ab_save(od_json, f = "~/orgs/a-b-street/abstreet/activity_leeds.json")



## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  # Regenerate json data
#  ab_save(od_json, "inst/extdata/activity_leeds.json")

## ---- eval=FALSE--------------------------------------------------------------
#  file.edit("scenario1.json")

## -----------------------------------------------------------------------------
od_sf_roundtrip = ab_sf("activity_leeds.json")
# Or in the file saved in the abstr package
# od_sf_roundtrip = ab_sf(json = system.file("extdata/activity_leeds.json", package = "abstr"))
identical(od_sf$geometry, od_sf_roundtrip$geometry) 

## ---- include=FALSE-----------------------------------------------------------
file.remove("scenario1.json")

## -----------------------------------------------------------------------------
head(sao_paulo_activity_sf_2)
sp_2_json = ab_json(sao_paulo_activity_sf_2, mode_column = "mode", scenario_name = "2-agents")

## ---- eval=FALSE--------------------------------------------------------------
#  ab_save(sp_2_json, "activity_sp_2.json")

## -----------------------------------------------------------------------------
head(sao_paulo_activity_sf_20)
sp_20_json = ab_json(sao_paulo_activity_sf_20, mode_column = "mode", scenario_name = "20-agents")

## ---- eval=FALSE--------------------------------------------------------------
#  ab_save(sp_20_json, "activity_sp_20.json") # save in current folder, or:
#  # save to directory where you cloned the abstreet repo
#  # (replace '~/orgs...' with the path to your local directory)
#  # ab_save(sp_20_json, "~/orgs/a-b-street/abstreet/activity_sp_20.json")

