## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(amp)

## -----------------------------------------------------------------------------
x_data <- matrix(rnorm(500), ncol = 5)
y_data <- rnorm(100) + 0.02 * x_data[, 2]
obs_data <- data.frame(y_data, x_data)

## -----------------------------------------------------------------------------
tc <- amp::test.control(n_peld_mc_samples = 50, pos_lp_norms = "2")
set.seed(10)
test_1 <- amp::mv_pn_test(obs_data = obs_data, param_est = amp::ic.pearson, 
                control = tc)
set.seed(20)
test_2 <- amp::mv_pn_test(obs_data = obs_data, param_est = amp::ic.pearson, 
                control = tc)
print(c(test_1$test_stat, test_2$test_stat))

## ---- fig.width = 8, fig.height = 6-------------------------------------------
mc_draws <- c(10, 50)
all_res <- list()
for (mc_draws in c(10, 50)) {
  set.seed(121)
  tc <- amp::test.control(n_peld_mc_samples = mc_draws, pos_lp_norms = 2, 
                          perf_meas = "est_acc")
  test_stat <- replicate(50, amp::mv_pn_test(obs_data = obs_data,
                            param_est = amp::ic.pearson,
                            control = tc)$test_stat)
  all_res[[as.character(mc_draws)]] <- 
    data.frame("mc_draws" = mc_draws, test_stat)
}
oldpar <- par(mfrow = c(1,2))
yl <- 25 
hist(all_res[[1]]$test_stat, main = "MC draws = 10",
     xlab = "Test Statistic", xlim = c(0, 1), ylim = c(0, yl), 
     breaks = seq(0, 1, 0.1)) 
hist(all_res[[2]]$test_stat, main = "MC draws = 50",
     xlab = "Test Statistic", xlim = c(0, 1),  ylim = c(0, yl), 
     breaks = seq(0, 1, 0.1))
par(oldpar)

