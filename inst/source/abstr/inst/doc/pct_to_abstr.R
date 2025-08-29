## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  ####
#  ####
#  #### AIM: Use PCT data to create scenario of change where commuting cycling levels increase and car journeys decrease which can be imported
#  ####      into A/B Street city simulation software. This method should be fully reproducible for all other pct_regions.
#  ####
#  ####

## -----------------------------------------------------------------------------
#  #### INSTALL PACKAGES ####
#  cran_pkgs = c("abstr", "pct", "osmextract", "sf", "tidyverse")
#  remotes::install_cran(cran_pkgs)
#  #### LOAD PACKAGES ####
#  library(abstr)
#  library(pct)
#  library(osmextract)
#  library(sf)
#  library(dplyr)

## -----------------------------------------------------------------------------
#  pct_regions$region_name

## -----------------------------------------------------------------------------
#  region_name = "devon"

## -----------------------------------------------------------------------------
#  lookup = pct::pct_regions_lookup
#  table(lookup$lad16nm[lookup$region_name == region_name])

## -----------------------------------------------------------------------------
#  lad_name = "Exeter"

## -----------------------------------------------------------------------------
#  ####    READ DATA ####
#  devon_zones = get_pct_zones(region = region_name, geography = "msoa") # get zone data
#  # filter for exeter
#  exeter_zones = devon_zones %>% filter(lad_name == lad_name) %>%
#    select(geo_code)
#  # get commute od data
#  exeter_commute_od = get_pct_lines(region = region_name, geography = "msoa") %>%
#    filter(lad_name1 == lad_name & lad_name2 == lad_name) # filter for exeter

## -----------------------------------------------------------------------------
#  exeter_commute_od = exeter_commute_od %>%
#    mutate(cycle_base = bicycle) %>%
#    mutate(walk_base = foot) %>%
#    mutate(transit_base = bus + train_tube) %>% # bunch of renaming -_-
#    mutate(drive_base = car_driver + car_passenger + motorbike + taxi_other) %>%
#    mutate(all_base = all) %>%
#    mutate(
#      # create new columns
#      pcycle_godutch_uptake = uptake_pct_godutch_2020(distance = rf_dist_km, gradient = rf_avslope_perc),
#      cycle_godutch_additional = pcycle_godutch_uptake * drive_base,
#      cycle_godutch = cycle_base + cycle_godutch_additional,
#      pcycle_godutch = cycle_godutch / all_base,
#      drive__godutch = drive_base - cycle_godutch_additional,
#      across(c(drive__godutch, cycle_godutch), round, 0),
#      all_go_dutch = drive__godutch + cycle_godutch + transit_base + walk_base
#    ) %>%
#    select(
#      # select variables for new df
#      geo_code1,
#      geo_code2,
#      cycle_base,
#      drive_base,
#      walk_base,
#      transit_base,
#      all_base,
#      all_go_dutch,
#      drive__godutch,
#      cycle_godutch,
#      cycle_godutch_additional,
#      pcycle_godutch
#    )

## -----------------------------------------------------------------------------
#  # sanity check: ensure total remains the same
#  # (this is not a dynamic model where population change is factored in)
#  identical(exeter_commute_od$all_base, exeter_commute_od$all_go_dutch)

## -----------------------------------------------------------------------------
#  ####    DOWNLOAD OSM BUILDING DATA ####
#  osm_polygons = osmextract::oe_read(
#    "https://download.geofabrik.de/europe/great-britain/england/devon-latest.osm.pbf",
#    # download osm buildings for region using geofabrik
#    layer = "multipolygons"
#  )
#  
#  building_types = c(
#    "yes",
#    "house",
#    "detached",
#    "residential",
#    "apartments",
#    "commercial",
#    "retail",
#    "school",
#    "industrial",
#    "semidetached_house",
#    "church",
#    "hangar",
#    "mobile_home",
#    "warehouse",
#    "office",
#    "college",
#    "university",
#    "public",
#    "garages",
#    "cabin",
#    "hospital",
#    "dormitory",
#    "hotel",
#    "service",
#    "parking",
#    "manufactured",
#    "civic",
#    "farm",
#    "manufacturing",
#    "floating_home",
#    "government",
#    "bungalow",
#    "transportation",
#    "motel",
#    "manufacture",
#    "kindergarten",
#    "house_boat",
#    "sports_centre"
#  )
#  osm_buildings  = osm_polygons %>%
#    filter(building %in% building_types) %>%
#    select(osm_way_id, name, building)
#  
#  osm_buildings_valid = osm_buildings[sf::st_is_valid(osm_buildings), ]
#  
#  exeter_osm_buildings_all = osm_buildings_valid[exeter_zones, ]

## -----------------------------------------------------------------------------
#  ####  JOIN OSM BUILDINGS WITH ZONE DATA ####
#  exeter_osm_buildings_all_joined = exeter_osm_buildings_all %>%
#    sf::st_join(exeter_zones)
#  
#  exeter_osm_buildings_sample = exeter_osm_buildings_all_joined %>%
#    filter(!is.na(osm_way_id))
#  
#  exeter_osm_buildings_tbl = exeter_osm_buildings_all %>%
#    filter(osm_way_id %in% exeter_osm_buildings_sample$osm_way_id)

## -----------------------------------------------------------------------------
#  set.seed(2021) # for reproducible builds
#  ####  LOGIC GATE ####
#  # Logic gate for go_dutch scenario of change, where cycling levels increase to a proportion reflecting the Netherlands.
#  # Switch to FALSE if you want census commuting OD
#  go_dutch = TRUE
#  if (go_dutch == TRUE) {
#    exeter_od = exeter_commute_od %>%
#      mutate(All = all_go_dutch) %>%
#      mutate(Bike = cycle_godutch) %>%
#      mutate(Transit = transit_base) %>%
#      mutate(Drive = drive_base) %>%
#      mutate(Walk = walk_base) %>%
#      select(geo_code1, geo_code2, All, Bike, Transit, Drive, Walk,geometry)
#  } else {
#    exeter_od = exeter_commute_od %>%
#      mutate(All = all_base) %>%
#      mutate(Bike = cycle_base) %>%
#      mutate(Drive = drive_base) %>%
#      mutate(Transit = transit_base) %>%
#      mutate(Walk = walk_base) %>%
#      select(geo_code1, geo_code2, All, Bike, Transit, Drive, Walk, geometry)
#  }

## -----------------------------------------------------------------------------
#  ####  GENERATE A/B STREET SCENARIO ####
#  output_sf = ab_scenario(
#    od = exeter_od,
#    zones = exeter_zones,
#    zones_d = NULL,
#    origin_buildings = exeter_osm_buildings_tbl,
#    destination_buildings = exeter_osm_buildings_tbl,
#    pop_var = 3,
#    time_fun = ab_time_normal,
#    output = "sf",
#    modes = c("Walk", "Bike", "Drive", "Transit")
#  )

## -----------------------------------------------------------------------------
#  #### SAVE JSON FILE ####
#  output_json = ab_json(output_sf, time_fun = ab_time_normal, scenario_name = "Go Dutch")
#  ab_save(output_json, f = "dutch.json")

## -----------------------------------------------------------------------------
#  # Upload the json file for future reference
#  piggyback::pb_upload("dutch.json")
#  piggyback::pb_download_url("dutch.json")

