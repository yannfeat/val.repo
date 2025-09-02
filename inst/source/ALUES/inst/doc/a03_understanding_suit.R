## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  suit(
#    crop,
#    terrain = NULL,
#    water = NULL,
#    temp = NULL,
#    mf = "triangular",
#    sow_month = NULL,
#    minimum = NULL,
#    maximum = "average",
#    interval = NULL,
#    sigma = NULL
#  )

## -----------------------------------------------------------------------------
library(ALUES)
banana_suit <- suit("banana", terrain=MarinduqueLT)
names(banana_suit)

## ---- eval=FALSE--------------------------------------------------------------
#  banana_suit[["terrain"]]
#  banana_suit[["soil"]]

## -----------------------------------------------------------------------------
names(banana_suit[["soil"]])

## -----------------------------------------------------------------------------
banana_suit[["soil"]][["Factors Evaluated"]]

## -----------------------------------------------------------------------------
d <- utils::data(package = "ALUES")
alues_data <- d$results[, "Item"]
crop_data <- regmatches(alues_data, gregexpr(paste0("^[A-Z]{2,}", collapse = "|"), alues_data))
crop_data <- unique(unlist(lapply(crop_data, function(x) substr(x, 1, nchar(x)-1))))
crop_data

## ---- error=TRUE--------------------------------------------------------------
potato_suit1 <- suit("sweet potato", terrain=MarinduqueLT)
potato_suit2 <- suit("potatosw", terrain=MarinduqueLT)

## -----------------------------------------------------------------------------
head(MarinduqueLT)

## -----------------------------------------------------------------------------
BANANATerrain
BANANASoil
BANANAWater
BANANATemp

## -----------------------------------------------------------------------------
banana_suit[["terrain"]]

## -----------------------------------------------------------------------------
banana_multi <- suit("banana", terrain=MarinduqueLT, water=MarinduqueWater, temp=MarinduqueTemp, sow_month=2)
names(banana_multi)

## -----------------------------------------------------------------------------
banana_suit[["terrain"]]
banana_suit[["water"]]
banana_suit[["temp"]]
lapply(banana_suit[["soil"]], function(x) head(x))

## -----------------------------------------------------------------------------
banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal")
head(banana_suit[["soil"]][["Suitability Score"]])
head(banana_suit[["soil"]][["Suitability Class"]])

## -----------------------------------------------------------------------------
banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal", interval=c(0, 0.3, 0.6, 0.9, 1))
head(banana_suit[["soil"]][["Suitability Score"]])
head(banana_suit[["soil"]][["Suitability Class"]])

## -----------------------------------------------------------------------------
banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal", interval="unbias")
head(banana_suit[["soil"]][["Suitability Score"]])
head(banana_suit[["soil"]][["Suitability Class"]])

## -----------------------------------------------------------------------------
banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal", interval="unbias")
banana_suit[["soil"]][["Factors Evaluated"]]

## -----------------------------------------------------------------------------
banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal", interval="unbias", maximum=c(60, 20, 9, 10))
banana_suit

## -----------------------------------------------------------------------------
MarinduqueLT2 <- MarinduqueLT[, 3:ncol(MarinduqueLT)]
banana_suit <- suit("banana", terrain=MarinduqueLT2, mf="trapezoidal", interval="unbias", maximum=c(60, 20, 9, 10))
head(banana_suit[["soil"]][["Suitability Score"]])
head(banana_suit[["soil"]][["Suitability Class"]])

