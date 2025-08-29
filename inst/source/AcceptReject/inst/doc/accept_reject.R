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
#  install.packages("AcceptReject")
#  
#  # or
#  
#  install.packages("remotes")
#  remotes::install_github("prdm0/AcceptReject", force = TRUE)
#  
#  # Load the package
#  library(AcceptReject)

## ----eval = FALSE-------------------------------------------------------------
#  accept_reject(
#    n = 1L,
#    continuous = TRUE,
#    f = NULL,
#    args_f = NULL,
#    f_base = NULL,
#    random_base = NULL,
#    args_f_base = NULL,
#    xlim = NULL,
#    c = NULL,
#    parallel = FALSE,
#    cores = NULL,
#    warning = TRUE,
#    ...
#  )

## -----------------------------------------------------------------------------
library(AcceptReject)

# Ensuring Reproducibility
set.seed(0)

# Generating observations
data <- AcceptReject::accept_reject(
  n = 1000L,
  f = dpois,
  continuous = FALSE,
  args_f = list(lambda = 0.7),
  xlim = c(0, 20),
  parallel = FALSE
)

# Viewing organized output with useful information
print(data)

# Calculating the true probability function for each observed value
values <- unique(data)
true_prob <- dpois(values, lambda = 0.7)

# Calculating the observed probability for each value in the observations vector
obs_prob <- table(data) / length(data)

# Plotting the probabilities and observations
plot(values, true_prob, type = "p", pch = 16, col = "blue",
     xlab = "x", ylab = "Probability", main = "Probability Function")

# Adding the observed probabilities
points(as.numeric(names(obs_prob)), obs_prob, pch = 16L, col = "red")
legend("topright", legend = c("True probability", "Observed probability"),
       col = c("blue", "red"), pch = 16L, cex = 0.8)
grid()

## -----------------------------------------------------------------------------
library(AcceptReject)

# Ensuring reproducibility
set.seed(0)

# Generating observations
data <- AcceptReject::accept_reject(
  n = 2000L,
  f = dbinom,
  continuous = FALSE,
  args_f = list(size = 5, prob = 0.5),
  xlim = c(0, 20),
  parallel = FALSE
)

# Viewing organized output with useful information
print(data)

# Calculating the true probability function for each observed value
values <- unique(data)
true_prob <- dbinom(values, size = 5, prob = 0.5)

# Calculating the observed probability for each value in the observations vector
obs_prob <- table(data) / length(data)

# Plotting the probabilities and observations
plot(values, true_prob, type = "p", pch = 16, col = "blue",
     xlab = "x", ylab = "Probability", main = "Probability Function")

# Adding the observed probabilities
points(as.numeric(names(obs_prob)), obs_prob, pch = 16L, col = "red")
legend("topright", legend = c("True probability", "Observed probability"),
       col = c("blue", "red"), pch = 16L, cex = 0.8)
grid()

## -----------------------------------------------------------------------------
library(AcceptReject)

# Ensuring reproducibility
set.seed(0)

# Generating observations
data <- AcceptReject::accept_reject(
  n = 2000L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4),
  parallel = FALSE
)

# Viewing organized output with useful information
print(data)

hist(
  data,
  main = "Generating Gaussian observations",
  xlab = "x",
  probability = TRUE,
  ylim = c(0, 0.4)
)

x <- seq(-4, 4, length.out = 500L)
y <- dnorm(x, mean = 0, sd = 1)
lines(x, y, col = "red", lwd = 2)
legend("topright", legend = "True density", col = "red", lwd = 2)

## -----------------------------------------------------------------------------
library(AcceptReject)
library(cowplot) # install.packages("cowplot")

# Ensuring reproducibility
set.seed(0)

simulation <- function(n){
  AcceptReject::accept_reject(
    n = n,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    parallel = FALSE
  )
}
# Inspecting
a <- plot(simulation(n = 250L))
b <- plot(simulation(n = 2500L))
c <- plot(simulation(n = 25000L))
d <- plot(simulation(n = 250000L))

plot_grid(a, b, c, d, nrow = 2L, labels = c("a", "b", "c", "d"))

## -----------------------------------------------------------------------------
library(AcceptReject)
library(cowplot) # install.packages("cowplot")
# Ensuring Reproducibility
set.seed(0)

simulation <- function(n){
  AcceptReject::accept_reject(
    n = n,
    f = dpois,
    continuous = FALSE,
    args_f = list(lambda = 0.7),
    xlim = c(0, 20),
    parallel = FALSE
  )
}

a <- plot(simulation(25L))
b <- plot(simulation(250L))
c <- plot(simulation(2500L))
d <- plot(simulation(25000L))

plot_grid(a, b, c, d, nrow = 2L, labels = c("a", "b", "c", "d"))

## -----------------------------------------------------------------------------
library(AcceptReject)

data <- accept_reject(
  n = 1000L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4)
)

# Creating a histogram
hist(data)

# Checking the size of the vector of observations
length(x)

## -----------------------------------------------------------------------------
library(AcceptReject)

data <- accept_reject(
  n = 100L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4)
)
attributes(data)

# Accessing the value c
attr(data, "c")

## -----------------------------------------------------------------------------
library(AcceptReject)

data <- accept_reject(
  n = 100L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4)
)
class(data)
print(data)

# Coercing the object into an atomic vector without attributes
data <- as.vector(data)
print(data)

