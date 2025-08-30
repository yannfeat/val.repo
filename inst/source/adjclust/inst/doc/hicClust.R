## ----skipNoHITC---------------------------------------------------------------
# IMPORTANT: this vignette can not be created if HiTC is not installed
if (!require("HiTC", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----loadLib, message=FALSE---------------------------------------------------
#  library("adjclust")

## ----loadData-----------------------------------------------------------------
#  load(system.file("extdata", "hic_imr90_40_XX.rda", package = "adjclust"))

## ----create-data-script-------------------------------------------------------
#  system.file("system/create_hic_chrXchrX.R", package="adjclust")

## ----mapHiC, message=FALSE----------------------------------------------------
#  HiTC::mapC(hic_imr90_40_XX)

## ----hicClust-HTCexp, message=FALSE-------------------------------------------
#  fit <- hicClust(hic_imr90_40_XX)

## ----binning, message=FALSE---------------------------------------------------
#  binned <- HiTC::binningC(hic_imr90_40_XX, binsize = 1e5)
#  fitB <- hicClust(binned)
#  fitB

## ----plotBinned, message=FALSE------------------------------------------------
#  HiTC::mapC(binned)

## ----dendro-------------------------------------------------------------------
#  plot(fitB, mode = "corrected")

## ----objectDesc---------------------------------------------------------------
#  head(cbind(fitB$merge, fitB$gains))

## ----session------------------------------------------------------------------
#  sessionInfo()

