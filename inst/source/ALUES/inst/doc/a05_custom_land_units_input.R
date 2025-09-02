## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
terrain_input <- data.frame(
  Flood = c(1, 2, 2, 2, 3),
  SlopeD = c(3, 4, 5, 1, 2),
  CFragm = c(10, 30, 50, 60, 40),
  SoilDpt = c(45, 60, 90, 70, 30)
)

## -----------------------------------------------------------------------------
library(ALUES)
AVOCADOTerrain
AVOCADOSoil
avocado_suit <- suit("avocado", terrain=terrain_input)
head(avocado_suit[["terrain"]][["Suitability Score"]])
head(avocado_suit[["terrain"]][["Suitability Class"]])
head(avocado_suit[["soil"]][["Suitability Score"]])
head(avocado_suit[["soil"]][["Suitability Class"]])

## -----------------------------------------------------------------------------
water_input <- data.frame(
  Apr = c(150, 140, 120),
  May = c(70, 90, 100),
  Jun = c(85, 90, 105)
)
water_input

## -----------------------------------------------------------------------------
RICEBRWater
water_suit <- suit("ricebr", water=water_input, sow_month=1)
water_suit

## -----------------------------------------------------------------------------
water_suit <- suit("ricebr", water=water_input, sow_month=3)
water_suit

## -----------------------------------------------------------------------------
temp_input <- data.frame(
  Sep = c(34.2, 35.5, 33.4),
  Oct = c(32.5, 34.2, 32.0),
  Nov = c(30.3, 32.2, 31.1)
)
RICEBRTemp
temp_suit <- suit("ricebr", temp=temp_input, sow_month=9)

## -----------------------------------------------------------------------------
temp_suit

