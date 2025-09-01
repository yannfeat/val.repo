## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(aion)
library(methods)

## -----------------------------------------------------------------------------
## Years since 753 BC (the traditional founding of Rome)
AUC <- new(
  Class = "GregorianCalendar",
  label = "AUC",               # Abbreviated label
  name = "Ab urbe condita",    # Name of the time scale
  epoch = 753,                 # Epoch from which years are counted
  direction = 1L               # Count years forwards from epoch
)

AUC

## -----------------------------------------------------------------------------
## Egyptian calendar
E <- setClass(
  Class = "EgyptianCalendar",
  prototype = list(
    name = "Egyptian",
    fixed = -272787,
    direction = 1L,
    year = 365
  ),
  contains = "TimeScale"
)

## -----------------------------------------------------------------------------
## Convert Egyptian dates to rata die
## NB: this method MUST return a RataDie object
setMethod(
  f = "fixed",
  signature = c(
    year = "numeric",
    month = "numeric",
    day = "numeric", 
    calendar = "EgyptianCalendar"
  ),
  definition = function(year, month, day, calendar) {
    rd <- calendar_fixed(calendar) + 
      365 * (year - 1) + 
      30 * (month - 1) + 
      day - 1
    
    as_fixed(rd)
  }
)

## Convert rata die to Egyptian dates
## NB: this method MUST return a data.frame
setMethod(
  f = "as_date",
  signature = c(object = "numeric", calendar = "EgyptianCalendar"),
  definition = function(object, calendar) {
    day <- object - calendar_fixed(calendar)
    year <- day %/% 365 + 1
    month <- (day %% 365) %/% 30 + 1
    day <- day - 365 * (year - 1) - 30 * (month - 1) + 1
    
    data.frame(year = year, month = month, day = day)
  }
)

## Convert rata die to Egyptian years
setMethod(
  f = "as_year",
  signature = c(object = "numeric", calendar = "EgyptianCalendar"),
  definition = function(object, calendar, ...) {
    (object - calendar_fixed(calendar)) %/% 365 + 1
  }
)

## -----------------------------------------------------------------------------
## Create a calendar object
cal <- E()

## Convert 161/7/15 in rata die
fixed(
  year = 161,
  month = 7,
  day = 15, 
  calendar = cal
)

## Convert -214193 r.d. to an Egyptian date
as_date(-214193, calendar = cal)

## -----------------------------------------------------------------------------
## Build a conversion function from Gregorian CE years to Egyptian years
Gregorian_to_Egyptian <- convert(CE(), E())

## Convert 2023 (Gregorian) to the Egyptian calendar
Gregorian_to_Egyptian(2023)

## -----------------------------------------------------------------------------
.CalibratedAges <- setClass(
  Class = "CalibratedAges",
  slots = c(
    ages = "numeric",     # Stores the radiocarbon ages to be calibrated
    errors = "numeric",   # Store the standard deviation of the radiocarbon ages
    curves = "character"  # Store the name of the calibration curve
  ),
  contains = "TimeSeries"
)

