## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(AdsorpR)
library(ggplot2)

## ----sample-data--------------------------------------------------------------
Ce <- c(1, 2, 3, 4, 5)
Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)

## ----langmuir-----------------------------------------------------------------
result_l <- langmuir_model(Ce, Qe)
print(result_l[1:2])        # Qmax and KL
print(result_l$`Model Summary`)
result_l$Plot

## ----freundlich---------------------------------------------------------------
result_f <- freundlich_model(Ce, Qe)
print(result_f[1:2])        # Kf and n
print(result_f$`Model Summary`)
result_f$Plot

## ----bet----------------------------------------------------------------------
result_b <- bet_model(Ce, Qe)
print(result_b[1:2])        # Qm and Cb
print(result_b$`Model Summary`)
result_b$Plot

## ----temkin-------------------------------------------------------------------
result_t <- temkin_model(Ce, Qe)
print(result_t[1:2])        # A and B
print(result_t$`Model Summary`)
result_t$Plot

## -----------------------------------------------------------------------------
Ce <- c(1, 2, 4, 6, 8, 10)
Qe <- c(0.9, 1.6, 2.3, 2.7, 2.9, 3.0)
result <- nonlinear_langmuir(Ce, Qe)
print(result$`Langmuir Qmax (mg/g)`)
print(result$`Langmuir KL (L/mg)`)
print(result$AIC)
print(result$`Pseudo R2`)
print(result$Plot)

## -----------------------------------------------------------------------------
Ce <- c(0.5, 1, 2, 4, 6, 8)
Qe <- c(0.3, 0.8, 1.6, 2.4, 2.9, 3.2)
result <- nonlinear_freundlich(Ce, Qe)
print(result$`Freundlich Kf`)
print(result$`Freundlich n`)
print(result$AIC)
print(result$`Pseudo R2`)
print(result$Plot)

## -----------------------------------------------------------------------------
Ce <- c(1, 2.5, 4, 5.5, 7)
Qe <- c(0.4, 1.0, 1.7, 2.3, 2.7)
result <- nonlinear_bet(Ce, Qe)
print(result$`BET Qm (mg/g)`)
print(result$`BET Cb`)
print(result$AIC)
print(result$`Pseudo R2`)
print(result$Plot)

## -----------------------------------------------------------------------------
Ce <- c(0.5, 1.5, 3, 4.5, 6)
Qe <- c(0.7, 1.3, 2.0, 2.4, 2.7)
result <- nonlinear_temkin(Ce, Qe)
print(result$`Temkin A`)
print(result$`Temkin B`)
print(result$AIC)
print(result$`Pseudo R2`)
print(result$Plot)

