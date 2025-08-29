## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
options(tibble.print_min = 6, tibble.print_max = 6)

modern_r <- getRversion() >= "4.1.0"

## -----------------------------------------------------------------------------
library(AcceptReject)
library(cowplot) # install.packages("cowplot")

# Ensuring reproducibility
set.seed(0)

# Inspecting
# Case a
a <- inspect(
  f = dweibull,
  args_f = list(shape = 2.1, scale = 2.2),
  f_base = dgamma,
  args_f_base = list(shape = 2.8, rate = 1.2),
  xlim = c(0, 10),
  c = 1.2
)

# Inspecting
# Case b
b <- inspect(
  f = dweibull,
  args_f = list(shape = 2.1, scale = 2.2),
  f_base = dgamma,
  args_f_base = list(shape = 2.9, rate = 2.5),
  xlim = c(0, 10),
  c = 1.4
)

plot_grid(a, b, nrow = 2L, labels = c("a", "b"))

## -----------------------------------------------------------------------------
library(AcceptReject)
library(tictoc) # install.packages("tictoc")

# Ensuring reproducibility
set.seed(0)

# Não especificando a função densidade de probabilidade base

tic()
case_1 <- accept_reject(
  n = 200e3L,
  continuous = TRUE,
  f = dweibull,
  args_f = list(shape = 2.1, scale = 2.2),
  xlim = c(0, 10)
)
toc()

# Specifying the base probability density function
tic()
case_2 <- accept_reject(
  n = 200e3L,
  continuous = TRUE,
  f = dweibull,
  args_f = list(shape = 2.1, scale = 2.2),
  f_base = dgamma,
  random_base = rgamma,
  args_f_base = list(shape = 2.8, rate = 1.2),
  xlim = c(0, 10),
  c = 1.2
)
toc()

# Visualizing the results
p1 <- plot(case_1)
p2 <- plot(case_2)

plot_grid(p1, p2, nrow = 2L)

