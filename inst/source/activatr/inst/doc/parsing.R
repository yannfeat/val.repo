## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----parse--------------------------------------------------------------------
library(activatr)

# Get the running_example.gpx file included with this package.
filename <- system.file(
  "extdata",
  "running_example.gpx.gz",
  package = "activatr"
)

df <- parse_gpx(filename)

## ----table, echo=FALSE, results='asis'----------------------------------------
knitr::kable(head(df, 5))

## ----parse_advanced-----------------------------------------------------------
df_advanced <- parse_gpx(filename, detail = "advanced")

## ----table_advanced, echo=FALSE, results='asis'-------------------------------
knitr::kable(head(df_advanced, 5))

## ----plot, fig.show = "hold", warning = FALSE, message = FALSE----------------
library(ggplot2)
library(dplyr)
ggplot(df_advanced) +
  geom_line(aes(x = time, y = hr), color = "red")
ggplot(filter(df_advanced, cad > 80)) +
  geom_density(aes(x = cad * 2), fill = "blue", bw = 1)

## ----sample-------------------------------------------------------------------
# Parsing as normal gets all of the rows, but takes longer
full_time <- system.time({
  df_full <- parse_gpx(filename)
})
nrow(df_full)
full_time

# Grabbing every hundredth data point runs much faster
sample_time <- system.time({
  df_sample <- parse_gpx(filename, every = 100)
})
nrow(df_sample)
sample_time

