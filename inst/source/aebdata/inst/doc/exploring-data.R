## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(aebdata)

## ----list-themes, eval = FALSE------------------------------------------------
#  list_themes()

## ----list-themes-knit, echo = FALSE-------------------------------------------
list_themes() |>
  knitr::kable(format = "html")

## ----list-series, eval = FALSE------------------------------------------------
#  list_series(theme_id = c(41,49))

## ----list-series-knit, echo = FALSE-------------------------------------------
list_series(theme_id = c(41,49)) |>
  knitr::kable(format = "html")

## ----get-series---------------------------------------------------------------
downloaded_series <- get_series(series_id = c(240, 241))

## ----download-141, eval = FALSE-----------------------------------------------
#  head(downloaded_series$`240`)

## ----download-141-knit, echo = FALSE------------------------------------------
head(downloaded_series$`240`) |>
  knitr::kable(format = "html")

