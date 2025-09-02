## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(amp)

## -----------------------------------------------------------------------------
x_data <- matrix(rnorm(5000), ncol = 5)
y_data <- rnorm(1000) + 0.02 * x_data[, 2]
obs_data <- data.frame(y_data, x_data)

## -----------------------------------------------------------------------------
amp::ic.pearson(obs_data, what = "est")
cor(y_data, x_data)

## -----------------------------------------------------------------------------
test_res <- amp::mv_pn_test(obs_data, param_est = amp::ic.pearson, control = amp::test.control())
hist(test_res$test_st_eld)
abline(v = test_res$pvalue, col = "red")

## -----------------------------------------------------------------------------
ic.mean <- function(observ, what = "both", control = NULL){
  if (!(what %in% c("ic", "est", "both"))) {
    stop("what must be one of ic (influence curve), est (estimate), or both")
  }
  ret <- list()
  if (what %in% c("ic", "both")) {
    col_means <- colMeans(x = observ)
  infl <- sweep(x = observ, MARGIN = 2,
                STATS = col_means, FUN = "-")
  ret$ic <- infl
  }
  if (what %in% c("est", "both")) {
    ret$est <- colMeans(x = observ)
  }
  return(ret) 
}

## -----------------------------------------------------------------------------
set.seed(100)
obs_data_mean1 <- matrix(rnorm(n = 500) +
                          rep(c(0, 0, 0, 0, 0.01), each = 100),
                        ncol = 5, byrow = FALSE)
res_1 <- amp::mv_pn_test(obs_data_mean1, param_est = ic.mean,
                          control = amp::test.control())

obs_data_mean2 <- matrix(rnorm(n = 500) +
                           rep(c(0, 0, 0.32, 0, 0.07), each = 100),
                         ncol = 5, byrow = FALSE)
res_2 <- amp::mv_pn_test(obs_data_mean2, param_est = ic.mean,
                          control = amp::test.control())
print(c(res_1$pvalue, res_2$pvalue))

## -----------------------------------------------------------------------------
## Correct usage
nested_fun1 <- function(x) return(x)
ic.mean1 <- function(observ, what = "both", control = NULL){
  if (!(what %in% c("ic", "est", "both"))) {
    stop("what must be one of ic (influence curve), est (estimate), or both")
  }
  ret <- list()
  if (what %in% c("ic", "both")) {
    mult <- nested_fun1(control$extra_arg)
    col_means <- mult * colMeans(x = observ)
    infl <- sweep(x = observ, MARGIN = 2,
                  STATS = col_means, FUN = "-")
    ret$ic <- infl
  }
  if (what %in% c("est", "both")) {
    ret$est <- colMeans(x = observ)
  }
  return(ret) 
}

test1 <- amp::mv_pn_test(obs_data_mean1, param_est = ic.mean1,
                         control = c(amp::test.control(), 
                                     "extra_arg" = 2))

## Incorrect usage
nested_fun2 <- function(x) return(x$extra_arg)
ic.mean2 <- function(observ, what = "both", control = NULL){
  if (!(what %in% c("ic", "est", "both"))) {
    stop("what must be one of ic (influence curve), est (estimate), or both")
  }
  ret <- list()
  if (what %in% c("ic", "both")) {
    mult <- nested_fun2(control)
    col_means <- mult * colMeans(x = observ)
    infl <- sweep(x = observ, MARGIN = 2,
                  STATS = col_means, FUN = "-")
    ret$ic <- infl
  }
  if (what %in% c("est", "both")) {
    ret$est <- colMeans(x = observ)
  }
  return(ret) 
}
test2 <- amp::mv_pn_test(obs_data_mean1, param_est = ic.mean2,
                         control = c(amp::test.control(), 
                                     "extra_arg" = 2))

