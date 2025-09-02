## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=FALSE, warning=FALSE, message=FALSE, out.width='100%'
)

## ----setup, echo=TRUE, eval=FALSE---------------------------------------------
#  library(AMPLE)
#  measuring_performance()

## ----start, fig.cap="The opening screen of the 'Measuring performance' app."----
knitr::include_graphics("meas_perf_start.png")

## ----hcr11, fig.cap="The result from running a single projection with no uncertainty with HCR 1."----
knitr::include_graphics("meas_perf_hcr1_1.png")

## ----hcr12, fig.cap="The result from running a single projection with biological variability with HCR 1."----
knitr::include_graphics("meas_perf_hcr1_2.png")

## ----hcr110, fig.cap="The result from running 10 replicates with biological variability with HCR 1."----
knitr::include_graphics("meas_perf_hcr1_10.png")

## ----hcr150, fig.cap="The performance indicators from running 50 replicates with biological uncertainty with HCR 1."----
knitr::include_graphics("meas_perf_hcr1_50pi.png")

