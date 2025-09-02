## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ALUES)
suit_banana <- suit("banana", terrain=MarinduqueLT)
head(suit_banana[["soil"]]$`Suitability Score`)
head(suit_banana[["soil"]]$`Suitability Class`)

## -----------------------------------------------------------------------------
osuit <- overall_suit(suit_banana[["soil"]], method="average")
head(osuit)

## -----------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  suppressWarnings(suit("banana", terrain=MarinduqueLT, interval="unbias"))
)
microbenchmark(
  suppressWarnings(suit("banana", terrain=LaoCaiLT, interval="unbias"))
)

