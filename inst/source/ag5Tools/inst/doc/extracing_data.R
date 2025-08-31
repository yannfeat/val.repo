## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

data("arusha_df", package = "ag5Tools")

head(arusha_df)




## ----eval = FALSE-------------------------------------------------------------
#  
#  library(ag5Tools)
#  
#  arusha_rainfall <- ag5_extract(coords = arusha_df,
#                                 variable = "Precipitation-Flux",
#                                 path = "D:/agera5_data/")
#  

## ----eval = FALSE-------------------------------------------------------------
#  arusha_rainfall <- ag5_extract(coords = example_df,
#                                 lon = "x",
#                                 lat = "y",
#                                 start_date = "planting_date",
#                                 end_date = "harvest_date",
#                                 variable = "Precipitation-Flux",
#                                 path = "D:/agera5_data/")
#  

