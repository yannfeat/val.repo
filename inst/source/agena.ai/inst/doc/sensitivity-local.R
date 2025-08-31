## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(agena.ai)

## -----------------------------------------------------------------------------
#  #setwd("/Users/user/repos/api-r")

## -----------------------------------------------------------------------------
#  model <- from_cmpx("CarCosts.cmpx")
#  network <- model$networks[[1]]

## -----------------------------------------------------------------------------
#  model$create_dataSet(id = "sa")

## -----------------------------------------------------------------------------
#  sa_config <- create_sensitivity_config(
#    target="total_cost",
#    sensitivity_nodes="*",
#    dataset="sa",
#    report_settings = list(summaryStats = c("mean", "variance"))
#  )

## -----------------------------------------------------------------------------
#  local_api_sensitivity(model, sa_config, 'local_sensitivity.json')

