## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
d <- utils::data(package = "ALUES")
alues_data <- d$results[, "Item"]
crop_data <- regmatches(alues_data, gregexpr(paste0("^[A-Z]{2,}", collapse = "|"), alues_data))
crop_data <- unique(unlist(lapply(crop_data, function(x) substr(x, 1, nchar(x)-1))))
crop_data

## -----------------------------------------------------------------------------
library(ALUES)
COFFEEARSoil

## -----------------------------------------------------------------------------
new_crop <- data.frame(matrix(nrow=0, ncol=ncol(COFFEEARSoil)))
names(new_crop) <- names(COFFEEARSoil)
new_crop

## -----------------------------------------------------------------------------
new_crop[1, "code"] <- "CFragm"
new_crop[1, 2:4] <- c(60, 40, 20)
new_crop

## -----------------------------------------------------------------------------
new_crop[2, "code"] <- "pHH2O"
new_crop[2, 2:7] <- c(4.5, 5.0, 5.1, 5.6, 6.2, 6.9)
new_crop

## -----------------------------------------------------------------------------
new_crop[2, "wts"] <- 2
new_crop

## -----------------------------------------------------------------------------
newcrop_suit <- suit(new_crop, terrain=MarinduqueLT)
lapply(newcrop_suit[["terrain"]], function (x) head(x))

