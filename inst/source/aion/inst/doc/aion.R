## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(aion)

## -----------------------------------------------------------------------------
## Create a calendar object
## (Gregorian Common Era)
calendar("CE")

## -----------------------------------------------------------------------------
## Common Era (Gregorian)
CE()

## Before Present (Gregorian)
BP()

## -----------------------------------------------------------------------------
## Get default calendar
get_calendar()

## Change default calendar to BP
set_calendar("BP")
get_calendar()

## Set it back to Gregorian Common Era
set_calendar("CE")
get_calendar()

## -----------------------------------------------------------------------------
## Convert 2000-02-29 (Gregorian) to rata die
fixed(2000, 02, 29, calendar = calendar("CE"))

## If days and months are missing, decimal years are expected
fixed(2000.161, calendar = calendar("CE"))

## -----------------------------------------------------------------------------
## Create a vector of 10 years BP (Gregorian)
## (every 20 years starting from 2000 BP)
(years <- seq(from = 2000, by = -20, length.out = 10))

## Convert years to rata die
(rd <- fixed(years, calendar = calendar("BP")))

## Convert back to Gregorian years
as_year(rd, calendar = calendar("CE"))  # Common Era
as_year(rd, calendar = calendar("BP"))  # Before Present
as_year(rd, calendar = calendar("b2k")) # Before 2000

## -----------------------------------------------------------------------------
format(rd) # Default calendar (Gregorian Common Era)
format(rd, prefix = "ka", calendar = calendar("BP"))

## -----------------------------------------------------------------------------
## Get ceramic counts (data from Husi 2022)
data("loire", package = "folio")

## Keep only variables whose total is at least 600
keep <- c("01f", "01k", "01L", "08e", "08t", "09b", "15i", "15q")

## Get time midpoints
mid <- rowMeans(loire[, c("lower", "upper")])

## Create time-series
(X <- series(
  object = loire[, keep],
  time = mid,
  calendar = calendar("AD")
))

## -----------------------------------------------------------------------------
## Time series duration
span(X) # Default: rata die
span(X, calendar = CE())

## Time of first observation
start(X) # Default: rata die
start(X, calendar = CE())

## Time of last observation
end(X) # Default: rata die
end(X, calendar = CE())

## Sampling times
time(X, calendar = BP())

## ----plot-multiple, fig.width=7, fig.height=5---------------------------------
## Multiple plot (default calendar)
plot(
  x = X, 
  type = "h" # histogram like vertical lines
)

## ----plot-single, fig.width=7, fig.height=3.5---------------------------------
## Extract the first series
Y <- X[, 1, ]

## Plot a single series
plot(
  Y,
  type = "h", # histogram like vertical lines
  calendar = b2k(), # b2k time scale
  panel.first = graphics::grid() # Add a grid
)
year_axis(side = 3, calendar = CE()) # Add a secondary time axis
mtext(format(CE()), side = 3, line = 3) # Add secondary axis title

