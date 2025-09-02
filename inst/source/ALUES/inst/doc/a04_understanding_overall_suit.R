## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ALUES)
banana_suit <- suit("banana", terrain=MarinduqueLT)
class(banana_suit[["terrain"]])
class(banana_suit[["soil"]])

## -----------------------------------------------------------------------------
ovsuit <- overall_suit(banana_suit[["soil"]])
head(ovsuit)

## -----------------------------------------------------------------------------
ovsuit <- overall_suit(banana_suit[["soil"]], method="average")
head(ovsuit)

## -----------------------------------------------------------------------------
ovsuit <- overall_suit(banana_suit[["soil"]], method="average", interval=c(0, 0.6, 0.7, 0.9, 1))
head(ovsuit)

