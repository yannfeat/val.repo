## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(adaptDiag)

ss <- binom_sample_size(alpha = 0.05, power = 0.9, p0 = 0.7, p1 = 0.824)
ss

## ----p_thresh-----------------------------------------------------------------
p_thresh <- seq(0.95, 0.995, 0.005)

## ----simulate, eval=FALSE-----------------------------------------------------
#  tab <- NULL
#  
#  for (i in 1:length(p_thresh)) {
#    fit_p <- multi_trial(
#      sens_true = 0.7,
#      spec_true = 0.963,
#      prev_true = 0.20,
#      endpoint = "sens",
#      sens_pg = 0.7,
#      spec_pg = NULL,
#      prior_sens = c(0.1, 0.1),
#      prior_spec = c(0.1, 0.1),
#      prior_prev = c(0.1, 0.1),
#      succ_sens = p_thresh[i],
#      n_at_looks = seq(100, 600, 50),
#      n_mc = 10000,
#      n_trials = 5000,
#      ncores = 8L)
#  
#    out <- summarise_trials(fit_p, min_pos = 35, fut = 0.05)
#    tab <- rbind(tab, out)
#  }

## ----load_results, echo=FALSE-------------------------------------------------
load("vignette-sims.rda")

## ----results, fig.height=5, fig.width=5---------------------------------------
plot(p_thresh, tab$power,
     xlab = "Probability success threshold",
     ylab = "Type I error",
     main = "",
     type = "b",
     bty  = "n")
grid()
abline(h = 0.05, col = 2)
abline(h = 0.05 + 1.96 * sqrt(0.05 * 0.95 / 5000),
       col = 2, lty = 2)
abline(h = 0.05 - 1.96 * sqrt(0.05 * 0.95 / 5000),
       col = 2, lty = 2)

## ----simulate_power, eval=FALSE-----------------------------------------------
#  power <- multi_trial(
#      sens_true = 0.824,
#      spec_true = 0.963,
#      prev_true = 0.20,
#      endpoint = "sens",
#      sens_pg = 0.7,
#      spec_pg = NULL,
#      prior_sens = c(0.1, 0.1),
#      prior_spec = c(0.1, 0.1),
#      prior_prev = c(0.1, 0.1),
#      succ_sens = 0.985,
#      n_at_looks = seq(100, 600, 50),
#      n_mc = 10000,
#      n_trials = 5000,
#      ncores = 8L)

## -----------------------------------------------------------------------------
summarise_trials(power, min_pos = 35, fut = 0.05)

