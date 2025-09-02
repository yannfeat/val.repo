## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=FALSE, warning=FALSE, message=FALSE, out.width='100%'
)

## ----setup, echo=TRUE, eval=FALSE---------------------------------------------
#  library(AMPLE)
#  comparing_performance()

## ----start, fig.cap="The opening screen of the 'Comparing performance' app."----
knitr::include_graphics("comp_perf_start.png")

## ----hcr1, fig.cap="The results from evaluating HCR 1."-----------------------
knitr::include_graphics("comp_perf_hcr1.png")

## ----compbar1, fig.cap="Using bar charts to compare the average values of performance indicators of three candidate HCRs."----
knitr::include_graphics("comp_perf_compbar1.png")

## ----compbar2, fig.cap="Using bar charts to compare the average values of only four performance indicators of three candidate HCRs."----
knitr::include_graphics("comp_perf_compbar2.png")

## ----compbar3, fig.cap="Using bar charts to compare the average values of only three performance indicators after dropping HCR 2."----
knitr::include_graphics("comp_perf_compbar3.png")

## ----compbox1, fig.cap="Using box plots to compare the range of expected values of three performance indicators for HCR 1 and HCR 3."----
knitr::include_graphics("comp_perf_compbox1.png")

