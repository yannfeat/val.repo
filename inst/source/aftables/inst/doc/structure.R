## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----class-demo---------------------------------------------------------------
library(aftables)
my_aftable <- as_aftable(demo_df)
class(my_aftable)

## ----print-demo---------------------------------------------------------------
my_aftable

## ----df-demo-no-eval----------------------------------------------------------
as.data.frame(my_aftable)

## ----is-demo------------------------------------------------------------------
is_aftable(my_aftable)

## ----summary-demo-------------------------------------------------------------
summary(my_aftable)

## ----tbl-sum-demo-------------------------------------------------------------
pillar::tbl_sum(my_aftable)

## ----wb-class-----------------------------------------------------------------
my_wb <- generate_workbook(my_aftable)
class(my_wb)

## ----print-wb-----------------------------------------------------------------
my_wb

