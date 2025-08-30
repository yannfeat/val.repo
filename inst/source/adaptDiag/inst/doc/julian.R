## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----param_pg, eval=FALSE-----------------------------------------------------
#  sens_pg <- 0.7
#  spec_pg <- 0.9
#  
#  succ_sens <- 0.985,
#  succ_spec <- 0.985,
#  
#  endpoint <- "both"

## ----param_true, eval=FALSE---------------------------------------------------
#  sens_true <- 0.824
#  spec_true <- 0.963
#  prev_true <- 0.2

## ----priors, eval=FALSE-------------------------------------------------------
#  prior_sens <- c(0.1, 0.1)
#  prior_spec <- c(0.1, 0.1)
#  prior_prev <- c(0.1, 0.1)

## ----sample_size, eval=FALSE--------------------------------------------------
#  n_at_looks <- seq.int(200, 700, by = 50)

## ----simulate, cache=TRUE-----------------------------------------------------
library(adaptDiag)

fit_power <- multi_trial(
  sens_true = 0.824,
  spec_true = 0.963,
  prev_true = 0.20,
  endpoint = "both",
  sens_pg = 0.7,
  spec_pg = 0.9,
  prior_sens = c(0.1, 0.1),
  prior_spec = c(0.1, 0.1),
  prior_prev = c(0.1, 0.1),
  succ_sens = 0.985,
  succ_spec = 0.985,
  n_at_looks = seq(200, 700, 50),
  n_mc = 10000,
  n_trials = 200,
  ncores = 1L)

## ----op_chars-----------------------------------------------------------------
summarise_trials(fit_power, min_pos = 30, fut = 0.05)

## ----simulate_type1, cache=FALSE----------------------------------------------
fit_type1 <- update(fit_power,
                    sens_true = 0.7,
                    spec_true = 0.9)

summarise_trials(fit_type1, min_pos = 30, fut = 0.05)

## ----grid, cache=FALSE, results=FALSE-----------------------------------------
tab <- NULL

for (i in 1:length(prev_true_vec)) {
  fit_power_i <- multi_trial(
  sens_true = 0.824,
  spec_true = 0.963,
  prev_true = prev_true_vec[i],
  endpoint = "both",
  sens_pg = 0.7,
  spec_pg = 0.9,
  prior_sens = c(0.1, 0.1),
  prior_spec = c(0.1, 0.1),
  prior_prev = c(0.1, 0.1),
  succ_sens = 0.985,
  succ_spec = 0.985,
  n_at_looks = seq(200, 700, 50),
  n_mc = 1000,
  n_trials = 100,
  ncores = 1L)
  
  out <- summarise_trials(fit_power_i, min_pos = 30, fut = 0.05)
  tab <- rbind(tab, out)
}

## ----plot, fig.height=5, fig.width=5------------------------------------------
plot(prev_true_vec, tab$power,
     xlab = "True prevalence",
     ylab = "Power",
     main = "Prevalence vs. power",
     type = "b")

