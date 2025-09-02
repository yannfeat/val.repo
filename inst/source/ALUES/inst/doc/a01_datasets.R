## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ALUES)
head(MarinduqueLT)
head(LaoCaiLT)

## -----------------------------------------------------------------------------
d <- utils::data(package = "ALUES")
alues_data <- d$results[, "Item"]
crop_data <- regmatches(alues_data, gregexpr(paste0("^[A-Z]{2,}", collapse = "|"), alues_data))
crop_data <- unique(unlist(lapply(crop_data, function(x) substr(x, 1, nchar(x)-1))))
crop_data

## -----------------------------------------------------------------------------
GUAVASoil
GUAVATemp
CINNAMONTerrain
CINNAMONWater

