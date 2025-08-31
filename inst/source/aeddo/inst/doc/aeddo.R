## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install_github, eval=FALSE-----------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("ssi-dk/aeddo")

## ----install_cran, eval=FALSE-------------------------------------------------
#  install.packages("aeddo")

## ----libraries----------------------------------------------------------------
library(aeddo)
library(ggplot2)

## ----load_MASS----------------------------------------------------------------
deaths <- MASS::deaths

## -----------------------------------------------------------------------------
plot(deaths)

## ----formula------------------------------------------------------------------
formula <- y ~ 1 + sin(2 * pi * m / 12) + cos(2 * pi * m / 12)

## ----preprocess_ts------------------------------------------------------------
# Extract timestamp
timestamp <- zoo::as.yearmon(
  stats::time(deaths)
)
# ... and observations
observations <- c(deaths)

## ----preprocess_month_in_year-------------------------------------------------
m <- as.integer(
  format(timestamp, "%m")
)

## ----preprocess_df------------------------------------------------------------
# Bundle up in data.frame
processed_deaths <- data.frame(
  time = timestamp,
  y = observations,
  n = 1,
  m = m
)

## ----k------------------------------------------------------------------------
k <- 24
sig_level <- 0.9

## ----aeddo_results------------------------------------------------------------
aeddo_results <- aeddo(
  data = processed_deaths,
  formula = formula,
  k = k,
  sig_level = sig_level,
  exclude_past_outbreaks = TRUE,
  init_theta = c(1, 0, 0, 1),
  lower = c(1e-6, -6, -6, 1e-6),
  upper = c(1e2, 6, 6, 3),
  method = "L-BFGS-B"
)

## ----aeddo_autoplot, eval=FALSE-----------------------------------------------
#  autoplot(aeddo_results)

## ----aeddo_viz----------------------------------------------------------------
plot(aeddo_results)

