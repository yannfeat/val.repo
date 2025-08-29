## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,include = FALSE-----------------------------------------------
library(acledR)
library(dplyr)

## ----out.width = "500px", echo = FALSE----------------------------------------
knitr::include_graphics("workflow.png")

## ----eval=FALSE---------------------------------------------------------------
# # Install acledR
# install.packages("acledR") # from CRAN
# 
# devtools::install_github("ACLED/acledR") # or from github.
# 
# # Load acledR
# library(acledR)

## ----eval=FALSE---------------------------------------------------------------
# acled_access(email = "email@example.com", key = "your_key") #  This is an example, you will need to input your credentials.

## ----eval=FALSE---------------------------------------------------------------
# argentinian_data <- acled_api(
#   # Country of interest
#   country = "Argentina",
#   # Earliest date for requested events
#   start_date ="2022-01-01",
#   # Last date for requested events
#   end_date = "2022-12-31",
#   # Request 'inter codes' in numeric rather than text form
#   inter_numeric = TRUE,
#   # Turn off acled_api() interactive prompt
#   prompt = FALSE
#   )

## ----eval = FALSE-------------------------------------------------------------
# new_data <- acled_update(acledR::acled_old_deletion_dummy,
#                          inter_numeric = TRUE,
#                          prompts = FALSE)

## -----------------------------------------------------------------------------
long_data <- acled_transform_longer(acledR::acled_old_dummy, type = "full_actors")

head(long_data)

## -----------------------------------------------------------------------------
wide_data <- acled_transform_wider(long_data, type = "full_actors")

head(wide_data)

## -----------------------------------------------------------------------------
transformed_data <- acled_transform_interaction(acledR::acled_old_dummy)

# Note the inter1 and inter2 columns
head(transformed_data)

