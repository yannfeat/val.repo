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

## ----eval = FALSE-------------------------------------------------------------
#  ## S3 method for class 'accept_reject'
#  qqplot(
#    x,
#    alpha = 0.5,
#    color_points = "#F890C2",
#    color_line = "#BB9FC9",
#    size_points = 1,
#    size_line = 1,
#    ...
#  )

## -----------------------------------------------------------------------------
library(AcceptReject)
library(cowplot)
x <- accept_reject(
  n = 2000L,
  f = dbinom,
  continuous = FALSE,
  args_f = list(size = 5, prob = 0.5),
  xlim = c(0, 5)
)
a <- plot(x)
b <- qqplot(x)
plot_grid(a, b, ncol = 2)

## -----------------------------------------------------------------------------
# For n = 1000
y <- accept_reject(
  n = 1000L,
  f = dbeta,
  continuous = TRUE,
  args_f = list(shape1 = 2, shape2 = 2),
  xlim = c(0, 1)
)

# For many points (scattermore is used):
z <- accept_reject(
  n = 11e3,
  f = dbeta,
  continuous = TRUE,
  args_f = list(shape1 = 2, shape2 = 2),
  xlim = c(0, 1)
)

# Gráficos
a <- plot(y)
b <- qqplot(y)
c <- plot(z)
d <- qqplot(z)
plot_grid(a, b, ncol = 2)
plot_grid(c, d, ncol = 2)

