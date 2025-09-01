## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
library(bibtex)
library(bookdown)

refs <- read.bib("references.bib")
keys <- names(refs)

## ----setup--------------------------------------------------------------------
# library(allMT)

## -----------------------------------------------------------------------------
# Uncomment below line before running the snippet
# library(allMT)
pat_data <- system.file("extdata/tmc_data/", "UPN_916.xls", package = "allMT")
dest_path <- getwd() # user may choose a different destination 
file.copy(pat_data, dest_path)

## -----------------------------------------------------------------------------
# Below code should work for any user independent of OS
# pat_data <- system.file("extdata/processed_data/", "UPN_916.csv", package = "allMT")
# pat_df <- utils::read.csv(pat_data, header = TRUE, sep = ",")
# # head(pat_df)


