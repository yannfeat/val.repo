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

## ----speed--------------------------------------------------------------------
df <- mutate_with_speed(df)

## ----speedtable, echo=FALSE, results='asis'-----------------------------------
knitr::kable(head(df, 5))

## ----pace---------------------------------------------------------------------
df$pace <- speed_to_mile_pace(df$speed)

## ----pacetable, echo=FALSE, results='asis'------------------------------------
knitr::kable(head(df, 5))

## ----paceformatter, warning = FALSE, message = FALSE, fig.show = "hold"-------
library(ggplot2)
library(dplyr)
library(lubridate)
ggplot(filter(df, as.numeric(pace) < 1200)) +
  geom_line(aes(x = time, y = as.numeric(pace)), color = "blue")
ggplot(filter(df, as.numeric(pace) < 1200)) +
  geom_line(aes(x = time, y = as.numeric(pace)), color = "blue") +
  scale_y_reverse(label = pace_formatter)

## ----leadlag, warning = FALSE, message = FALSE--------------------------------
df <- mutate_with_speed(df, lead = 10, lag = 10)
df$pace <- speed_to_mile_pace(df$speed)
ggplot(filter(df, as.numeric(pace) < 1200)) +
  geom_line(aes(x = time, y = as.numeric(pace)), color = "blue") +
  scale_y_reverse(label = pace_formatter) +
  xlab("Time") +
  ylab("Pace")

