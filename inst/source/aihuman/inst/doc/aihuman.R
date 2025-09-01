## ----setup_hide, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6, fig.width = 9,
  message = FALSE
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(aihuman)

## ----data1--------------------------------------------------------------------
data("synth")
str(synth)
synth[1:6, ]
unique(synth$D)

## ----data2--------------------------------------------------------------------
data("psa_synth")
str(psa_synth)
psa_synth[1:6, ]

## ----data3--------------------------------------------------------------------
data("hearingdate_synth")
str(hearingdate_synth)
hearingdate_synth[1:6]

## ----plotstackedbar-----------------------------------------------------------
# Call the data
data(psa_synth)
# Treated group (PSA provided)
p.treated <- PlotStackedBar(psa_synth[psa_synth$Z == 1, ],
  d.colors = c("grey80", "grey60", "grey30", "grey10"),
  d.labels = c("signature", "small", "middle", "large"),
  legend.position = "bottom"
)
# Control group (PSA not provided)
p.control <- PlotStackedBar(psa_synth[psa_synth$Z == 0, ],
  d.colors = c("grey80", "grey60", "grey30", "grey10"),
  d.labels = c("signature", "small", "middle", "large"),
  legend.position = "bottom"
)

p.dmf <- PlotStackedBarDMF(psa_synth,
  d.colors = c("grey80", "grey60", "grey30", "grey10"),
  d.labels = c("signature", "small", "middle", "large"),
  dmf.label = "DMF"
)

# For more details
# ?PlotStackedBar

# Example
p.treated$p.fta

## ----plotstackedbar_plot, eval = FALSE----------------------------------------
# # To plot three figures together
# library(ggplot2)
# library(gridExtra)
# 
# mylegend <- g_legend(p.control$p.nvca)
# 
# # Treated
# grid.arrange(
#   arrangeGrob(p.control$p.fta + theme(legend.position = "none"),
#     p.control$p.nca + theme(legend.position = "none"),
#     p.control$p.nvca + theme(legend.position = "none"),
#     p.dmf$p.treat + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# 
# # Control
# grid.arrange(
#   arrangeGrob(p.control$p.fta + theme(legend.position = "none"),
#     p.control$p.nca + theme(legend.position = "none"),
#     p.control$p.nvca + theme(legend.position = "none"),
#     p.dmf$p.control + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )

## ----plotstackedbar_rep, eval = FALSE-----------------------------------------
# library(aihuman)
# data(PSAdata)
# library(tidyverse)
# library(gridExtra)
# 
# # Figure 1
# p.treated <- PlotStackedBar(PSAdata %>% filter(Z == 1), legend.position = "bottom")
# p.control <- PlotStackedBar(PSAdata %>% filter(Z == 0), legend.position = "bottom")
# p.dmf <- PlotStackedBarDMF(PSAdata, dmf.category = c("signature bond", "cash bond"))
# mylegend <- g_legend(p.treated$p.nvca)
# pdf("figs/stackedbar.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(p.treated$p.fta + theme(legend.position = "none"),
#     p.treated$p.nca + theme(legend.position = "none"),
#     p.treated$p.nvca + theme(legend.position = "none"),
#     p.dmf$p.treat + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()
# pdf("figs/stackedbar_control.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(p.control$p.fta + theme(legend.position = "none"),
#     p.control$p.nca + theme(legend.position = "none"),
#     p.control$p.nvca + theme(legend.position = "none"),
#     p.dmf$p.control + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()
# 
# # Figure S1
# S0.t <- PlotStackedBar(PSAdata %>% filter(Z == 1 & Sex == 0), legend.position = "bottom")
# S0.c <- PlotStackedBar(PSAdata %>% filter(Z == 0 & Sex == 0), legend.position = "bottom")
# S0.dmf <- PlotStackedBarDMF(PSAdata %>% filter(Sex == 0),
#   dmf.category = c("signature bond", "cash bond")
# )
# pdf("figs/stackedbar_F.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(S0.t$p.fta + theme(legend.position = "none"),
#     S0.t$p.nca + theme(legend.position = "none"),
#     S0.t$p.nvca + theme(legend.position = "none"),
#     S0.dmf$p.treat + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()
# pdf("figs/stackedbar_control_F.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(S0.c$p.fta + theme(legend.position = "none"),
#     S0.c$p.nca + theme(legend.position = "none"),
#     S0.c$p.nvca + theme(legend.position = "none"),
#     S0.dmf$p.control + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()
# 
# # Figure S3
# S1W1.t <- PlotStackedBar(PSAdata %>% filter(Z == 1 & Sex == 1 & White == 1), legend.position = "bottom")
# S1W1.c <- PlotStackedBar(PSAdata %>% filter(Z == 0 & Sex == 1 & White == 1), legend.position = "bottom")
# S1W1.dmf <- PlotStackedBarDMF(PSAdata %>% filter(Sex == 1 & White == 1),
#   dmf.category = c("signature bond", "cash bond")
# )
# pdf("figs/stackedbar_WM.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(S1W1.t$p.fta + theme(legend.position = "none"),
#     S1W1.t$p.nca + theme(legend.position = "none"),
#     S1W1.t$p.nvca + theme(legend.position = "none"),
#     S1W1.dmf$p.treat + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()
# pdf("figs/stackedbar_control_WM.pdf", height = 6, width = 22)
# grid.arrange(
#   arrangeGrob(S1W1.c$p.fta + theme(legend.position = "none"),
#     S1W1.c$p.nca + theme(legend.position = "none"),
#     S1W1.c$p.nvca + theme(legend.position = "none"),
#     S1W1.dmf$p.control + theme(legend.position = "none"),
#     nrow = 1
#   ),
#   mylegend,
#   nrow = 2, heights = c(10, 1)
# )
# dev.off()

## ----itt----------------------------------------------------------------------
# Call the data
data(synth)

# Subgroup index
subgroup_synth <- list(
  1:nrow(synth),
  which(synth$Sex == 0),
  which(synth$Sex == 1),
  which(synth$Sex == 1 & synth$White == 0),
  which(synth$Sex == 1 & synth$White == 1)
)

# Compute Diff-in-Means on decision
res_dec <- CalDIMsubgroup(synth, subgroup = subgroup_synth)
# Plot the results
PlotDIMdecisions(res_dec,
  decision.labels = c("signature", "small cash", "middle cash", "large cash"),
  col.values = c("grey60", "grey30", "grey6", "grey1"),
  shape.values = c(16, 17, 15, 18)
)

# Synthetic data for outcome
synth_fta <- synth_nca <- synth_nvca <- synth
set.seed(123)
synth_fta$Y <- sample(0:1, 1000, replace = T)
synth_nca$Y <- sample(0:1, 1000, replace = T)
synth_nvca$Y <- sample(0:1, 1000, replace = T)
# Compute Diff-in-Means on outcome
res_fta <- CalDIMsubgroup(synth_fta, subgroup = subgroup_synth)
res_nca <- CalDIMsubgroup(synth_nca, subgroup = subgroup_synth)
res_nvca <- CalDIMsubgroup(synth_nvca, subgroup = subgroup_synth)
# Plot the results
PlotDIMoutcomes(res_fta, res_nca, res_nvca, y.max = 0.3)

## ----itt_rep, eval = FALSE----------------------------------------------------
# # Figure 2
# data(FTAdata)
# data(NCAdata)
# data(NVCAdata)
# 
# # Subgroup index
# subgroup_data <- list(
#   1:nrow(FTAdata),
#   which(FTAdata$Sex == 0),
#   which(FTAdata$Sex == 1),
#   which(FTAdata$Sex == 1 & FTAdata$White == 0),
#   which(FTAdata$Sex == 1 & FTAdata$White == 1)
# )
# # Compute Diff-in-Means on decision
# res_dec <- CalDIMsubgroup(FTAdata, subgroup = subgroup_data)
# # Plot the results
# pdf("figs/itt_decisions.pdf", width = 9, height = 5)
# PlotDIMdecisions(res_dec, y.max = 0.17)
# dev.off()
# # Compute Diff-in-Means on outcome
# res_fta <- CalDIMsubgroup(FTAdata, subgroup = subgroup_data)
# res_nca <- CalDIMsubgroup(NCAdata, subgroup = subgroup_data)
# res_nvca <- CalDIMsubgroup(NVCAdata, subgroup = subgroup_data)
# # Plot the results
# pdf("figs/itt_outcomes.pdf", width = 9, height = 5)
# PlotDIMoutcomes(res_fta, res_nca, res_nvca,
#   y.max = 0.17, top.margin = 0.02, bottom.margin = 0.02,
#   label.position = c("top", "bottom", "top")
# )
# dev.off()

## ----itt_age_rep, eval = FALSE------------------------------------------------
# # Figure S5
# # Subgroup index
# subgroup_age <- list(
#   which(FTAdata$Age < 23),
#   which(FTAdata$Age >= 23 & FTAdata$Age < 29),
#   which(FTAdata$Age >= 29 & FTAdata$Age < 36),
#   which(FTAdata$Age >= 36 & FTAdata$Age < 46),
#   which(FTAdata$Age >= 46)
# )
# # Compute Diff-in-Means on decision
# res_age <- CalDIMsubgroup(FTAdata,
#   subgroup = subgroup_age,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# )
# # Plot the results
# pdf("figs/itt_decisions_age.pdf", width = 9, height = 5)
# PlotDIMdecisions(res_age)
# dev.off()
# # Compute Diff-in-Means on outcome
# res_fta_age <- CalDIMsubgroup(FTAdata,
#   subgroup = subgroup_age,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# )
# res_nca_age <- CalDIMsubgroup(NCAdata,
#   subgroup = subgroup_age,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# )
# res_nvca_age <- CalDIMsubgroup(NVCAdata,
#   subgroup = subgroup_age,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# )
# # Plot the results
# pdf("figs/itt_outcomes_age.pdf", width = 9, height = 5)
# PlotDIMoutcomes(res_fta_age, res_nca_age, res_nvca_age,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~"),
#   top.margin = 0.02, bottom.margin = 0.02,
#   label.position = c("top", "bottom", "top")
# )
# dev.off()

## ----aievalmcmc---------------------------------------------------------------
data(synth)
sample_mcmc <- AiEvalmcmc(data = synth, n.mcmc = 10)
# increase n.mcmc in your analysis

## ----calapce------------------------------------------------------------------
subgroup_synth <- list(
  1:nrow(synth),
  which(synth$Sex == 0),
  which(synth$Sex == 1),
  which(synth$Sex == 1 & synth$White == 0),
  which(synth$Sex == 1 & synth$White == 1)
)
sample_apce <- CalAPCE(
  data = synth,
  mcmc.re = sample_mcmc,
  subgroup = subgroup_synth
)
# Or using parallel computing (results are the same):
# sample_apce = CalAPCEparallel(data = synth,
#                               mcmc.re = sample_mcmc,
#                               subgroup = subgroup_synth,
#                               burnin = 0,
#                               size = 2)

## ----summaryapce--------------------------------------------------------------
sample_apce_summary <- APCEsummary(sample_apce[["APCE.mcmc"]])

## ----plotapce-----------------------------------------------------------------
PlotAPCE(sample_apce_summary,
  y.max = 0.25,
  decision.labels = c("signature", "small cash", "middle cash", "large cash"),
  shape.values = c(16, 17, 15, 18),
  col.values = c("blue", "black", "red", "brown", "purple"),
  label = FALSE
)

## ----apce_rep, eval = FALSE---------------------------------------------------
# ## Main analysis
# # MCMC
# library(coda)
# PSADiag_ordinal <- function(data, rho = 0,
#                             ## mcmc parameters
#                             n.mcmc = 5 * 10^4, verbose = TRUE, out.length = 500) {
#   set.seed(111)
#   mcmc1 <- AiEvalmcmc(data, rho = rho, n.mcmc = n.mcmc, verbose = TRUE, out.length = out.length)
#   set.seed(222)
#   mcmc2 <- AiEvalmcmc(data, rho = rho, n.mcmc = n.mcmc, verbose = TRUE, out.length = out.length)
#   set.seed(333)
#   mcmc3 <- AiEvalmcmc(data, rho = rho, n.mcmc = n.mcmc, verbose = TRUE, out.length = out.length)
#   set.seed(444)
#   mcmc4 <- AiEvalmcmc(data, rho = rho, n.mcmc = n.mcmc, verbose = TRUE, out.length = out.length)
#   set.seed(555)
#   mcmc5 <- AiEvalmcmc(data, rho = rho, n.mcmc = n.mcmc, verbose = TRUE, out.length = out.length)
# 
#   mcmc.PSA <- mcmc.list(mcmc1, mcmc2, mcmc3, mcmc4, mcmc5)
#   return(mcmc.PSA)
# }
# 
# PSA.ordinal.diag.fta <- PSADiag_ordinal(FTAdata, n.mcmc = 50000, verbose = TRUE, out.length = 500)
# PSA.ordinal.diag.nca <- PSADiag_ordinal(NCAdata, n.mcmc = 50000, verbose = TRUE, out.length = 500)
# PSA.ordinal.diag.nvca <- PSADiag_ordinal(NVCAdata, n.mcmc = 50000, verbose = TRUE, out.length = 500)
# 
# FTAmcmc <- lapply(PSA.ordinal.diag.fta, function(x) x[-(1:floor(0.5 * dim(x)[1])), ])
# FTAmcmc <- do.call("rbind", FTAmcmc)
# FTAmcmc <- mcmc(FTAmcmc)
# 
# NCAmcmc <- lapply(PSA.ordinal.diag.nca, function(x) x[-(1:floor(0.5 * dim(x)[1])), ])
# NCAmcmc <- do.call("rbind", NCAmcmc)
# NCAmcmc <- mcmc(NCAmcmc)
# 
# NVCAmcmc <- lapply(PSA.ordinal.diag.nvca, function(x) x[-(1:floor(0.5 * dim(x)[1])), ])
# NVCAmcmc <- do.call("rbind", NVCAmcmc)
# NVCAmcmc <- mcmc(NVCAmcmc)
# 
# # APCE
# subg <- list(
#   1:nrow(FTAdata),
#   which(FTAdata$Sex == 0),
#   which(FTAdata$Sex == 1),
#   which(FTAdata$Sex == 1 & FTAdata$White == 0),
#   which(FTAdata$Sex == 1 & FTAdata$White == 1)
# )
# 
# FTAace <- CalAPCEparallel(FTAdata, FTAmcmc, subgroup = subg, rho = 0, burnin = 0, size = 5)
# FTAqoi <- APCEsummary(FTAace[["APCE.mcmc"]])
# 
# NCAace <- CalAPCEparallel(NCAdata, NCAmcmc, subgroup = subg, rho = 0, burnin = 0, size = 5)
# NCAqoi <- APCEsummary(NCAace[["APCE.mcmc"]])
# 
# NVCAace <- CalAPCEparallel(NVCAdata, NVCAmcmc, subgroup = subg, rho = 0, burnin = 0, size = 5)
# NVCAqoi <- APCEsummary(NVCAace[["APCE.mcmc"]])
# 
# # Figure 4
# pdf("figs/qoi_fta.pdf", width = 9, height = 3)
# PlotAPCE(FTAqoi,
#   y.max = 0.166, top.margin = 0.05, bottom.margin = 0.05,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nca.pdf", width = 9, height = 3)
# PlotAPCE(NCAqoi, y.max = 0.166, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nvca.pdf", width = 9, height = 3.5)
# PlotAPCE(NVCAqoi, y.max = 0.166, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# dev.off()

## ----apce_age_rep, eval = FALSE-----------------------------------------------
# # APCE
# subg.age <- list(
#   which(FTAdata$Age < 23),
#   which(FTAdata$Age >= 23 & FTAdata$Age < 29),
#   which(FTAdata$Age >= 29 & FTAdata$Age < 36),
#   which(FTAdata$Age >= 36 & FTAdata$Age < 46),
#   which(FTAdata$Age >= 46)
# )
# subg.age.lab <- c("17~22", "23~28", "29~35", "36~45", "46~")
# FTAace.age <- CalAPCEparallel(FTAdata, FTAmcmc,
#   subgroup = subg.age,
#   name.group = subg.age.lab,
#   rho = 0, burnin = 0, size = 5
# )
# FTAqoi.age <- APCEsummary(FTAace.age[["APCE.mcmc"]])
# 
# NCAace.age <- CalAPCEparallel(NCAdata, NCAmcmc,
#   subgroup = subg.age,
#   name.group = subg.age.lab,
#   rho = 0, burnin = 0, size = 5
# )
# NCAqoi.age <- APCEsummary(NCAace.age[["APCE.mcmc"]])
# 
# NVCAace.age <- CalAPCEparallel(NVCAdata, NVCAmcmc,
#   subgroup = subg.age,
#   name.group = subg.age.lab,
#   rho = 0, burnin = 0, size = 5
# )
# NVCAqoi.age <- APCEsummary(NVCAace.age[["APCE.mcmc"]])
# 
# # Figure S7
# pdf("figs/qoi_fta_age.pdf", width = 9, height = 3)
# PlotAPCE(FTAqoi.age,
#   y.max = 0.124, top.margin = 0.03, bottom.margin = 0.03,
#   label.position = c("top", "bottom", "top", "bottom"),
#   name.group = subg.age.lab
# ) +
#   labs(title = "Failure to Appear (FTA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nca_age.pdf", width = 9, height = 3)
# PlotAPCE(NCAqoi.age, y.max = 0.124, label = FALSE, name.group = subg.age.lab) +
#   labs(title = "New Criminal Activity (NCA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nvca_age.pdf", width = 9, height = 3.5)
# PlotAPCE(NVCAqoi.age, y.max = 0.124, label = FALSE, name.group = subg.age.lab) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# dev.off()

## ----strata-------------------------------------------------------------------
sample_ps <- CalPS(sample_apce$P.R.mcmc)
PlotPS(sample_ps, col.values = c("blue", "black", "red", "brown", "purple"), label = FALSE)

## ----strata_rep, eval=FALSE---------------------------------------------------
# # P.R
# ps_fta <- CalPS(FTAace[["P.R.mcmc"]])
# ps_nca <- CalPS(NCAace[["P.R.mcmc"]])
# ps_nvca <- CalPS(NVCAace[["P.R.mcmc"]])
# # Figure 3
# pdf("figs/er_fta.pdf", width = 8, height = 6)
# PlotPS(ps_fta, y.max = 1, top.margin = 0.08, label.size = 5.2) +
#   labs(title = "Failure to Appear (FTA)")
# dev.off()
# pdf("figs/er_nca.pdf", width = 8, height = 6)
# PlotPS(ps_nca, y.max = 1, top.margin = 0.08, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)")
# dev.off()
# pdf("figs/er_nvca.pdf", width = 8, height = 6)
# PlotPS(ps_nvca, y.max = 1, top.margin = 0.08, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# dev.off()

## ----strata_age_rep, eval=FALSE-----------------------------------------------
# # P.R
# ps_fta.age <- CalPS(FTAace.age[["P.R.mcmc"]])
# ps_nca.age <- CalPS(NCAace.age[["P.R.mcmc"]])
# ps_nvca.age <- CalPS(NVCAace.age[["P.R.mcmc"]])
# # Figure S7
# pdf("figs/er_fta_age.pdf", width = 8, height = 6)
# PlotPS(ps_fta.age, y.max = 1, top.margin = 0.08, label.size = 5.2) +
#   labs(title = "Failure to Appear (FTA)")
# dev.off()
# pdf("figs/er_nca_age.pdf", width = 8, height = 6)
# PlotPS(ps_nca.age, y.max = 1, top.margin = 0.08, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)")
# dev.off()
# pdf("figs/er_nvca_age.pdf", width = 8, height = 6)
# PlotPS(ps_nvca.age, y.max = 1, top.margin = 0.08, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# dev.off()

## ----fair---------------------------------------------------------------------
sample_fair <- CalFairness(sample_apce)
PlotFairness(sample_fair, y.max = 0.4, y.min = -0.4, r.labels = c("Safe", "Preventable 1", "Preventable 2", "Preventable 3", "Risky"))

## ----fair_rep, eval=FALSE-----------------------------------------------------
# # Fairness
# resfta1 <- CalFairness(FTAace, attr = c(2, 3))
# resfta2 <- CalFairness(FTAace, attr = c(4, 5))
# resnca1 <- CalFairness(NCAace, attr = c(2, 3))
# resnca2 <- CalFairness(NCAace, attr = c(4, 5))
# resnvca1 <- CalFairness(NVCAace, attr = c(2, 3))
# resnvca2 <- CalFairness(NVCAace, attr = c(4, 5))
# 
# # Figure S5
# p1 <- PlotFairness(resfta1, top.margin = 0.02, y.max = 0.3) +
#   labs(title = "Failure to Appear (FTA)") + ylab("maximal sungroup difference")
# p4 <- PlotFairness(resfta2, top.margin = 0.02, y.max = 0.3) +
#   labs(title = "Failure to Appear (FTA)") + ylab("maximal sungroup difference")
# p2 <- PlotFairness(resnca1, top.margin = 0.02, y.max = 0.3, label = F) +
#   labs(title = "New Criminal Activity (NCA)")
# p5 <- PlotFairness(resnca2, top.margin = 0.02, y.max = 0.3, label = F) +
#   labs(title = "New Criminal Activity (NCA)")
# p3 <- PlotFairness(resnvca1, top.margin = 0.02, y.max = 0.3, label = F) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# p6 <- PlotFairness(resnvca2, top.margin = 0.02, y.max = 0.3, label = F) +
#   labs(title = "New Violent Criminal Activity (NVCA)")
# 
# pdf("figs/fair_gender.pdf", height = 5, width = 24)
# cowplot::plot_grid(p1, p2, p3, nrow = 1)
# dev.off()
# 
# pdf("figs/fair_race.pdf", height = 5, width = 24)
# cowplot::plot_grid(p4, p5, p6, nrow = 1)
# dev.off()

## ----optimal, eval = FALSE----------------------------------------------------
# # ?CalOptimalDecision # Please check the details
# synth_dmf <- sample(0:1, nrow(synth), replace = TRUE) # random dmf recommendation
# sample_optd_ci <- CalOptimalDecision(
#   data = synth,
#   mcmc.re = sample_mcmc,
#   c0.ls = seq(0, 5, 1), c1.ls = seq(0, 5, 1),
#   dmf = synth_dmf,
#   include.utility.diff.mcmc = TRUE,
#   # because of the above argument, the output is now a list
#   size = 2
# )
# sample_optd <- sample_optd_ci$res.i
# # Suppose we want to plot the proportion of cases for which *cash bond*
# # (either d1, d2 or d3) is optimal
# sample_optd$cash <- sample_optd$d1 + sample_optd$d2 + sample_optd$d3
# # Specify the column name of decision to be plotted, which is "cash" in this case.
# PlotOptimalDecision(sample_optd, "cash")

## ----optimal_rep, eval=FALSE--------------------------------------------------
# # Optimal Decision
# optd_fta_ci <- CalOptimalDecision(
#   data = FTAdata, mcmc.re = FTAmcmc,
#   c0.ls = seq(0, 10, 0.5), c1.ls = seq(0, 3, 0.1),
#   dmf = PSAdata$DMF, size = 5,
#   include.utility.diff.mcmc = TRUE
# )
# optd_nca_ci <- CalOptimalDecision(
#   data = NCAdata, mcmc.re = NCAmcmc,
#   c0.ls = seq(0, 10, 0.5), c1.ls = seq(0, 3, 0.1),
#   dmf = PSAdata$DMF, size = 5,
#   include.utility.diff.mcmc = TRUE
# )
# optd_nvca_ci <- CalOptimalDecision(
#   data = NVCAdata, mcmc.re = NVCAmcmc,
#   c0.ls = seq(0, 10, 0.5), c1.ls = seq(0, 3, 0.1),
#   dmf = PSAdata$DMF, size = 5,
#   include.utility.diff.mcmc = TRUE
# )
# optd_fta <- optd_fta_ci$res.i
# optd_nca <- optd_nca_ci$res.i
# optd_nvca <- optd_nvca_ci$res.i
# optd_fta$cash <- optd_fta$d1 + optd_fta$d2
# optd_nca$cash <- optd_nca$d1 + optd_nca$d2
# optd_nvca$cash <- optd_nvca$d1 + optd_nvca$d2
# 
# # Figure 6
# p1 <- PlotOptimalDecision(res = optd_fta, colname.d = "cash", idx = which(PSAdata$DMF == 0)) +
#   labs(
#     title = "Failure to Appear (FTA)", x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p2 <- PlotOptimalDecision(res = optd_nca, colname.d = "cash", idx = which(PSAdata$DMF == 0)) +
#   labs(title = "New Criminal Activity (NCA)", x = expression("Cost of NCA (" * c[0] * ")"), y = NULL)
# p3 <- PlotOptimalDecision(
#   res = optd_nvca, colname.d = "cash", idx = which(PSAdata$DMF == 0),
#   label.place = label_placement_fraction(0.9)
# ) +
#   labs(
#     title = "New Violent Criminal Activity (NVCA)",
#     x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL
#   )
# p4 <- PlotOptimalDecision(res = optd_fta, colname.d = "cash", idx = which(PSAdata$DMF == 1)) +
#   labs(
#     x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p5 <- PlotOptimalDecision(res = optd_nca, colname.d = "cash", idx = which(PSAdata$DMF == 1)) +
#   labs(x = expression("Cost of NCA (" * c[0] * ")"), y = NULL)
# p6 <- PlotOptimalDecision(
#   res = optd_nvca, colname.d = "cash", idx = which(PSAdata$DMF == 1),
#   label.place = label_placement_fraction(0.9)
# ) +
#   labs(x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL)
# 
# 
# pdf("figs/optd_combined_signature.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p1, p2, p3, nrow = 1)
# dev.off()
# 
# pdf("figs/optd_combined_cash.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p4, p5, p6, nrow = 1)
# dev.off()
# 
# # Figure S16
# p1 <- PlotOptimalDecision(res = optd_fta, colname.d = "cash", idx = which(PSAdata$FTAScore <= 4)) +
#   labs(
#     title = "Failure to Appear (FTA)", subtitle = "FTA Score: 1 - 4",
#     x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p2 <- PlotOptimalDecision(res = optd_nca, colname.d = "cash", idx = which(PSAdata$NCAScore <= 4)) +
#   labs(
#     title = "New Criminal Activity (NCA)", subtitle = "NCA Score: 1 - 4",
#     x = "NCA Score: 1 - 4", y = NULL
#   )
# p3 <- PlotOptimalDecision(
#   res = optd_nvca, colname.d = "cash", idx = which(PSAdata$NVCAFlag == 0),
#   label.place = label_placement_fraction(0.9)
# ) +
#   labs(
#     title = "New Violent Criminal Activity (NVCA)", subtitle = "NVCA Flag: 0",
#     x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL
#   )
# p4 <- PlotOptimalDecision(res = optd_fta, colname.d = "cash", idx = which(PSAdata$FTAScore > 4)) +
#   labs(
#     subtitle = "FTA Score: 5 - 6", x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p5 <- PlotOptimalDecision(res = optd_nca, colname.d = "cash", idx = which(PSAdata$NCAScore > 4)) +
#   labs(subtitle = "NCA Score: 5 - 6", x = expression("Cost of NCA (" * c[0] * ")"), y = NULL)
# p6 <- PlotOptimalDecision(
#   res = optd_nvca, colname.d = "cash", idx = which(PSAdata$NVCAFlag == 1),
#   label.place = label_placement_fraction(0.9)
# ) +
#   labs(subtitle = "NVCA Flag: 1", x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL)
# 
# 
# pdf("figs/optd_separate_signature.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p1, p2, p3, nrow = 1)
# dev.off()
# 
# pdf("figs/optd_separate_cash.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p4, p5, p6, nrow = 1)
# dev.off()

## ----comp, eval=FALSE---------------------------------------------------------
# PlotUtilityDiff(sample_optd_ci$res.i)
# PlotUtilityDiffCI(sample_optd_ci$res.mcmc)

## ----comp_rep, eval=FALSE-----------------------------------------------------
# # Figure 7
# p1 <- PlotUtilityDiff(optd_fta, which(PSAdata$Z == 1)) +
#   labs(
#     title = "Failure to Appear (FTA)",
#     x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p2 <- PlotUtilityDiff(optd_nca, which(PSAdata$Z == 1)) +
#   labs(
#     title = "New Criminal Activity (NCA)",
#     x = expression("Cost of NCA (" * c[0] * ")"), y = NULL
#   )
# p3 <- PlotUtilityDiff(optd_nvca, which(PSAdata$Z == 1)) +
#   labs(
#     title = "New Violent Criminal Activity (NVCA)",
#     x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL
#   )
# p4 <- PlotUtilityDiff(optd_fta, which(PSAdata$Z == 0)) +
#   labs(
#     x = expression("Cost of FTA (" * c[0] * ")"),
#     y = expression("Cost of unnecessarily harsh decision (" * c[1] * ")")
#   )
# p5 <- PlotUtilityDiff(optd_nca, which(PSAdata$Z == 0)) +
#   labs(x = expression("Cost of NCA (" * c[0] * ")"), y = NULL)
# p6 <- PlotUtilityDiff(optd_nvca, which(PSAdata$Z == 0)) +
#   labs(x = expression("Cost of NVCA (" * c[0] * ")"), y = NULL)
# 
# pdf("figs/utility_treatment.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p1, p2, p3, nrow = 1)
# dev.off()
# 
# pdf("figs/utility_control.pdf", width = 16, height = 5.2)
# cowplot::plot_grid(p4, p5, p6, nrow = 1)
# dev.off()
# 
# # Figure S17
# p.fta <- PlotUtilityDiffCI(optd_fta_ci$res.mcmc)
# p.nca <- PlotUtilityDiffCI(optd_nca_ci$res.mcmc)
# p.nvca <- PlotUtilityDiffCI(optd_nvca_ci$res.mcmc)
# p1 <- p.fta$p.treated + labs(
#   title = "Failure to Appear (FTA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of FTA (" * c[0] * ")")
# )
# p2 <- p.nca$p.treated + labs(
#   title = "New Criminal Activity (NCA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of NCA (" * c[0] * ")")
# )
# p3 <- p.nvca$p.treated + labs(
#   title = "New Violent Criminal Activity (NVCA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of NVCA (" * c[0] * ")")
# )
# p4 <- p.fta$p.control + labs(
#   title = "Failure to Appear (FTA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of FTA (" * c[0] * ")")
# )
# p5 <- p.nca$p.control + labs(
#   title = "New Criminal Activity (NCA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of NCA (" * c[0] * ")")
# )
# p6 <- p.nvca$p.control + labs(
#   title = "New Violent Criminal Activity (NVCA)",
#   y = "Difference in the Expected Utility",
#   x = expression("Cost of NVCA (" * c[0] * ")")
# )
# 
# pdf("figs/utility_treat_ci.pdf", width = 18, height = 6)
# cowplot::plot_grid(p1, p2, p3, nrow = 1)
# dev.off()
# 
# pdf("figs/utility_control_ci.pdf", width = 18, height = 6)
# cowplot::plot_grid(p4, p5, p6, nrow = 1)
# dev.off()

## ----crt, eval=FALSE----------------------------------------------------------
# # CRT
# data(hearingdate_synth)
# crt <- SpilloverCRT(
#   D = synth$D,
#   Z = synth$Z,
#   CourtEvent_HearingDate = hearingdate_synth,
#   n = 100
# ) # increase the permutation size to 10000
# PlotSpilloverCRT(crt)
# 
# # Power analysis
# crt_power <- SpilloverCRTpower(
#   D = synth$D,
#   Z = synth$Z,
#   CourtEvent_HearingDate = hearingdate_synth
# )
# PlotSpilloverCRTpower(crt_power)

## ----crt_rep, eval=FALSE------------------------------------------------------
# data(HearingDate)
# crt_data <- data.frame(
#   D = FTAdata$D,
#   Z = FTAdata$Z,
#   CourtEvent_HearingDate = HearingDate
# )
# # CRT
# crt <- SpilloverCRT(
#   D = crt_data$D,
#   Z = crt_data$Z,
#   CourtEvent_HearingDate = crt_data$CourtEvent_HearingDate,
#   n = 10000
# )
# # Figure S8
# pdf("figs/hist_crt.pdf", width = 4, height = 6)
# PlotSpilloverCRT(crt)
# dev.off()
# 
# # Power analysis
# crt_power <- SpilloverCRTpower(
#   D = crt_data$D,
#   Z = crt_data$Z,
#   CourtEvent_HearingDate = crt_data$CourtEvent_HearingDate,
#   size = 5,
#   n = 1000,
#   m = 1000,
#   cand_omegaZtilde = seq(-1.5, 1.5, by = 0.5)
# )
# # Figure S9
# pdf("figs/crt_power_prop.pdf", width = 4, height = 6)
# PlotSpilloverCRTpower(crt_power)
# dev.off()

## ----freq---------------------------------------------------------------------
synth$SexWhite <- synth$Sex * synth$White
freq_apce <- CalAPCEipw(synth)
boot_apce <- BootstrapAPCEipw(synth, rep = 10)
# subgroup analysis
data_s0 <- subset(synth, synth$Sex == 0, select = -c(Sex, SexWhite))
freq_s0 <- CalAPCEipw(data_s0)
boot_s0 <- BootstrapAPCEipw(data_s0, rep = 10)
data_s1 <- subset(synth, synth$Sex == 1, select = -c(Sex, SexWhite))
freq_s1 <- CalAPCEipw(data_s1)
boot_s1 <- BootstrapAPCEipw(data_s1, rep = 10)
data_s1w0 <- subset(synth, synth$Sex == 1 & synth$White == 0, select = -c(Sex, White, SexWhite))
freq_s1w0 <- CalAPCEipw(data_s1w0)
boot_s1w0 <- BootstrapAPCEipw(data_s1w0, rep = 10)
data_s1w1 <- subset(synth, synth$Sex == 1 & synth$White == 1, select = -c(Sex, White, SexWhite))
freq_s1w1 <- CalAPCEipw(data_s1w1)
boot_s1w1 <- BootstrapAPCEipw(data_s1w1, rep = 10)

freq_apce_summary <- APCEsummaryipw(
  freq_apce, freq_s0, freq_s1,
  freq_s1w0, freq_s1w1,
  boot_apce, boot_s0, boot_s1,
  boot_s1w0, boot_s1w0
)
PlotAPCE(freq_apce_summary,
  y.max = 0.25,
  decision.labels = c("signature", "small cash", "middle cash", "large cash"),
  shape.values = c(16, 17, 15, 18),
  col.values = c("blue", "black", "red", "brown", "purple"), label = FALSE
)

## ----freq_rep, eval=FALSE-----------------------------------------------------
# set.seed(123) # seed for bootstrap
# ## Frequentist approach
# # FTA
# freq_apce <- CalAPCEipw(FTAdata)
# boot_apce <- BootstrapAPCEipw(FTAdata, rep = 1000)
# data_s0 <- subset(FTAdata, FTAdata$Sex == 0, select = -c(Sex, SexWhite))
# freq_s0 <- CalAPCEipw(data_s0)
# boot_s0 <- BootstrapAPCEipw(data_s0, rep = 1000)
# data_s1 <- subset(FTAdata, FTAdata$Sex == 1, select = -c(Sex, SexWhite))
# freq_s1 <- CalAPCEipw(data_s1)
# boot_s1 <- BootstrapAPCEipw(data_s1, rep = 1000)
# data_s1w0 <- subset(FTAdata, FTAdata$Sex == 1 & FTAdata$White == 0, select = -c(Sex, White, SexWhite))
# freq_s1w0 <- CalAPCEipw(data_s1w0)
# boot_s1w0 <- BootstrapAPCEipw(data_s1w0, rep = 1000)
# data_s1w1 <- subset(FTAdata, FTAdata$Sex == 1 & FTAdata$White == 1, select = -c(Sex, White, SexWhite))
# freq_s1w1 <- CalAPCEipw(data_s1w1)
# boot_s1w1 <- BootstrapAPCEipw(data_s1w1, rep = 1000)
# fta_apce_summary <- APCEsummaryipw(
#   freq_apce, freq_s0, freq_s1,
#   freq_s1w0, freq_s1w1,
#   boot_apce, boot_s0, boot_s1,
#   boot_s1w0, boot_s1w0
# )
# # NCA
# freq_apce <- CalAPCEipw(NCAdata)
# boot_apce <- BootstrapAPCEipw(NCAdata, rep = 1000)
# data_s0 <- subset(NCAdata, NCAdata$Sex == 0, select = -c(Sex, SexWhite))
# freq_s0 <- CalAPCEipw(data_s0)
# boot_s0 <- BootstrapAPCEipw(data_s0, rep = 1000)
# data_s1 <- subset(NCAdata, NCAdata$Sex == 1, select = -c(Sex, SexWhite))
# freq_s1 <- CalAPCEipw(data_s1)
# boot_s1 <- BootstrapAPCEipw(data_s1, rep = 1000)
# data_s1w0 <- subset(NCAdata, NCAdata$Sex == 1 & NCAdata$White == 0, select = -c(Sex, White, SexWhite))
# freq_s1w0 <- CalAPCEipw(data_s1w0)
# boot_s1w0 <- BootstrapAPCEipw(data_s1w0, rep = 1000)
# data_s1w1 <- subset(NCAdata, NCAdata$Sex == 1 & NCAdata$White == 1, select = -c(Sex, White, SexWhite))
# freq_s1w1 <- CalAPCEipw(data_s1w1)
# boot_s1w1 <- BootstrapAPCEipw(data_s1w1, rep = 1000)
# nca_apce_summary <- APCEsummaryipw(
#   freq_apce, freq_s0, freq_s1,
#   freq_s1w0, freq_s1w1,
#   boot_apce, boot_s0, boot_s1,
#   boot_s1w0, boot_s1w0
# )
# 
# # Figure S10
# pdf("figs/qoi_fta_freq.pdf", width = 9, height = 3)
# PlotAPCE(fta_freq_apce_summary,
#   y.max = 0.23, top.margin = 0.05, bottom.margin = 0.05,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nca_freq.pdf", width = 9, height = 3.5)
# PlotAPCE(nca_freq_apce_summary, y.max = 0.23, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)")
# dev.off()
# 
# ## Random effects
# full.demographics <- "Sex + White + SexWhite + Age"
# full.other.cov <- "PendingChargeAtTimeOfOffense + NCorNonViolentMisdemeanorCharge + ViolentMisdemeanorCharge + ViolentFelonyCharge + NonViolentFelonyCharge + PriorMisdemeanorConviction + PriorFelonyConviction + PriorViolentConviction + PriorSentenceToIncarceration + PriorFTAInPast2Years + PriorFTAOlderThan2Years + Staff_ReleaseRecommendation + D"
# 
# mod <- paste0("Y ~ ", full.demographics, " + ", full.other.cov)
# mod.s <- paste0("Y ~ White + Age + ", full.other.cov)
# mod.sw <- paste0("Y ~ Age + ", full.other.cov)
# 
# re.mod <- "~ 1|CourtEvent_HearingDate"
# 
# # FTA
# freq_apce <- CalAPCEipwRE(FTAdata, fixed = mod, random = re.mod)
# boot_apce <- BootstrapAPCEipwRE(FTAdata, rep = 1000, fixed = mod, random = re.mod)
# data_s0 <- subset(FTAdata, FTAdata$Sex == 0, select = -c(Sex, SexWhite))
# freq_s0 <- CalAPCEipwRE(data_s0, fixed = mod.s, random = re.mod)
# boot_s0 <- BootstrapAPCEipwRE(data_s0, rep = 1000, fixed = mod.s, random = re.mod)
# data_s1 <- subset(FTAdata, FTAdata$Sex == 1, select = -c(Sex, SexWhite))
# freq_s1 <- CalAPCEipwRE(data_s1, fixed = mod.s, random = re.mod)
# boot_s1 <- BootstrapAPCEipwRE(data_s1, rep = 1000, fixed = mod.s, random = re.mod)
# data_s1w0 <- subset(FTAdata, FTAdata$Sex == 1 & FTAdata$White == 0, select = -c(Sex, White, SexWhite))
# freq_s1w0 <- CalAPCEipwRE(data_s1w0, fixed = mod.sw, random = re.mod)
# boot_s1w0 <- BootstrapAPCEipwRE(data_s1w0, rep = 1000, fixed = mod.sw, random = re.mod)
# data_s1w1 <- subset(FTAdata, FTAdata$Sex == 1 & FTAdata$White == 1, select = -c(Sex, White, SexWhite))
# freq_s1w1 <- CalAPCEipwRE(data_s1w1, fixed = mod.sw, random = re.mod)
# boot_s1w1 <- BootstrapAPCEipwRE(data_s1w1, rep = 1000, fixed = mod.sw, random = re.mod)
# fta_re_apce_summary <- APCEsummaryipw(
#   freq_apce, freq_s0, freq_s1,
#   freq_s1w0, freq_s1w1,
#   boot_apce, boot_s0, boot_s1,
#   boot_s1w0, boot_s1w0
# )
# # NCA
# freq_apce <- CalAPCEipwRE(NCAdata, fixed = mod, random = re.mod)
# boot_apce <- BootstrapAPCEipwRE(NCAdata, rep = 1000, fixed = mod, random = re.mod)
# data_s0 <- subset(NCAdata, NCAdata$Sex == 0, select = -c(Sex, SexWhite))
# freq_s0 <- CalAPCEipwRE(data_s0, fixed = mod.s, random = re.mod)
# boot_s0 <- BootstrapAPCEipwRE(data_s0, rep = 1000, fixed = mod.s, random = re.mod)
# data_s1 <- subset(NCAdata, NCAdata$Sex == 1, select = -c(Sex, SexWhite))
# freq_s1 <- CalAPCEipwRE(data_s1, fixed = mod.s, random = re.mod)
# boot_s1 <- BootstrapAPCEipwRE(data_s1, rep = 1000, fixed = mod.s, random = re.mod)
# data_s1w0 <- subset(NCAdata, NCAdata$Sex == 1 & NCAdata$White == 0, select = -c(Sex, White, SexWhite))
# freq_s1w0 <- CalAPCEipwRE(data_s1w0, fixed = mod.sw, random = re.mod)
# boot_s1w0 <- BootstrapAPCEipwRE(data_s1w0, rep = 1000, fixed = mod.sw, random = re.mod)
# data_s1w1 <- subset(NCAdata, NCAdata$Sex == 1 & NCAdata$White == 1, select = -c(Sex, White, SexWhite))
# freq_s1w1 <- CalAPCEipwRE(data_s1w1, fixed = mod.sw, random = re.mod)
# boot_s1w1 <- BootstrapAPCEipwRE(data_s1w1, rep = 1000, fixed = mod.sw, random = re.mod)
# nca_re_apce_summary <- APCEsummaryipw(
#   freq_apce, freq_s0, freq_s1,
#   freq_s1w0, freq_s1w1,
#   boot_apce, boot_s0, boot_s1,
#   boot_s1w0, boot_s1w0
# )
# 
# # Figure S11
# pdf("figs/qoi_fta_freq_re.pdf", width = 9, height = 3)
# PlotAPCE(fta_freq_apce_summary,
#   y.max = 0.34, top.margin = 0.1, bottom.margin = 0.1,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nca_freq_re.pdf", width = 9, height = 3.5)
# PlotAPCE(nca_freq_apce_summary, y.max = 0.34, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)")
# dev.off()
# 
# ## Age subgroup
# # FTA
# data_age1 <- subset(FTAdata, FTAdata$Age <= 22)
# freq_age1 <- CalAPCEipw(data_age1)
# boot_age1 <- BootstrapAPCEipw(data_age1, rep = 1000)
# data_age2 <- subset(FTAdata, FTAdata$Age > 22 & FTAdata$Age <= 28)
# freq_age2 <- CalAPCEipw(data_age2)
# boot_age2 <- BootstrapAPCEipw(data_age2, rep = 1000)
# data_age3 <- subset(FTAdata, FTAdata$Age > 28 & FTAdata$Age <= 35)
# freq_age3 <- CalAPCEipw(data_age3)
# boot_age3 <- BootstrapAPCEipw(data_age3, rep = 1000)
# data_age4 <- subset(FTAdata, FTAdata$Age > 35 & FTAdata$Age <= 45)
# freq_age4 <- CalAPCEipw(data_age4)
# boot_age4 <- BootstrapAPCEipw(data_age4, rep = 1000)
# data_age5 <- subset(FTAdata, FTAdata$Age > 45)
# freq_age5 <- CalAPCEipw(data_age5)
# boot_age5 <- BootstrapAPCEipw(data_age5, rep = 1000)
# fta_apce_summary <- APCEsummaryipw(
#   freq_age1, freq_age2, freq_age3,
#   freq_age4, freq_age5,
#   boot_age1, boot_age2, boot_age3,
#   boot_age4, boot_age5
# )
# # NCA
# data_age1 <- subset(NCAdata, NCAdata$Age <= 22)
# freq_age1 <- CalAPCEipw(data_age1)
# boot_age1 <- BootstrapAPCEipw(data_age1, rep = 1000)
# data_age2 <- subset(NCAdata, NCAdata$Age > 22 & NCAdata$Age <= 28)
# freq_age2 <- CalAPCEipw(data_age2)
# boot_age2 <- BootstrapAPCEipw(data_age2, rep = 1000)
# data_age3 <- subset(NCAdata, NCAdata$Age > 28 & NCAdata$Age <= 35)
# freq_age3 <- CalAPCEipw(data_age3)
# boot_age3 <- BootstrapAPCEipw(data_age3, rep = 1000)
# data_age4 <- subset(NCAdata, NCAdata$Age > 35 & NCAdata$Age <= 45)
# freq_age4 <- CalAPCEipw(data_age4)
# boot_age4 <- BootstrapAPCEipw(data_age4, rep = 1000)
# data_age5 <- subset(NCAdata, NCAdata$Age > 45)
# freq_age5 <- CalAPCEipw(data_age5)
# boot_age5 <- BootstrapAPCEipw(data_age5, rep = 1000)
# fta_apce_summary <- APCEsummaryipw(
#   freq_age1, freq_age2, freq_age3,
#   freq_age4, freq_age5,
#   boot_age1, boot_age2, boot_age3,
#   boot_age4, boot_age5
# )
# # Figure S12
# pdf("figs/qoi_fta_freq_age.pdf", width = 9, height = 3)
# PlotAPCE(fta_freq_apce_summary,
#   y.max = 0.25, top.margin = 0.13, bottom.margin = 0.09,
#   label.position = c("top", "bottom", "top", "bottom"),
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# ) +
#   labs(title = "Failure to Appear (FTA)") +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/qoi_nca_freq_age.pdf", width = 9, height = 3.5)
# PlotAPCE(nca_freq_apce_summary,
#   y.max = 0.25, label = FALSE,
#   name.group = c("17~22", "23~28", "29~35", "36~45", "46~")
# ) +
#   labs(title = "New Criminal Activity (NCA)")
# dev.off()

## ----sens_rep, eval=FALSE-----------------------------------------------------
# ## rho = 0.05
# ## Main analysis
# # MCMC
# FTAmcmc <- AiEvalmcmc(FTAdata, rho = 0.05, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NCAmcmc <- AiEvalmcmc(NCAdata, rho = 0.05, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NVCAmcmc <- AiEvalmcmc(NVCAdata, rho = 0.05, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# 
# # APCE
# FTAace <- CalAPCEparallel(FTAdata, FTAmcmc, subgroup = subg, rho = 0.05, burnin = 2 / 3, size = 5)
# FTAqoi <- APCEsummary(FTAace[["APCE.mcmc"]])
# 
# NCAace <- CalAPCEparallel(NCAdata, NCAmcmc, subgroup = subg, rho = 0.05, burnin = 2 / 3, size = 5)
# NCAqoi <- APCEsummary(NCAace[["APCE.mcmc"]])
# 
# NVCAace <- CalAPCEparallel(NVCAdata, NVCAmcmc, subgroup = subg, rho = 0.05, burnin = 2 / 3, size = 5)
# NVCAqoi <- APCEsummary(NVCAace[["APCE.mcmc"]])
# 
# # Figure S13
# pdf("figs/sens_fta_005.pdf", width = 9, height = 3)
# PlotAPCE(FTAqoi,
#   y.max = 0.18, top.margin = 0.05, bottom.margin = 0.05,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)", subtitle = expression(paste(rho, "= 0.05"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nca_005.pdf", width = 9, height = 3)
# PlotAPCE(NCAqoi, y.max = 0.18, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)", subtitle = expression(paste(rho, "= 0.05"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nvca_005.pdf", width = 9, height = 3.5)
# PlotAPCE(NVCAqoi, y.max = 0.3, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)", subtitle = expression(paste(rho, "= 0.05")))
# dev.off()
# 
# ## rho = 0.1
# ## Main analysis
# # MCMC
# FTAmcmc <- AiEvalmcmc(FTAdata, rho = 0.1, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NCAmcmc <- AiEvalmcmc(NCAdata, rho = 0.1, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NVCAmcmc <- AiEvalmcmc(NVCAdata, rho = 0.1, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# 
# # APCE
# FTAace <- CalAPCEparallel(FTAdata, FTAmcmc, subgroup = subg, rho = 0.1, burnin = 2 / 3, size = 5)
# FTAqoi <- APCEsummary(FTAace[["APCE.mcmc"]])
# 
# NCAace <- CalAPCEparallel(NCAdata, NCAmcmc, subgroup = subg, rho = 0.1, burnin = 2 / 3, size = 5)
# NCAqoi <- APCEsummary(NCAace[["APCE.mcmc"]])
# 
# NVCAace <- CalAPCEparallel(NVCAdata, NVCAmcmc, subgroup = subg, rho = 0.1, burnin = 2 / 3, size = 5)
# NVCAqoi <- APCEsummary(NVCAace[["APCE.mcmc"]])
# 
# # Figure S14
# pdf("figs/sens_fta_01.pdf", width = 9, height = 3)
# PlotAPCE(FTAqoi,
#   y.max = 0.19, top.margin = 0.05, bottom.margin = 0.05,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)", subtitle = expression(paste(rho, "= 0.1"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nca_01.pdf", width = 9, height = 3)
# PlotAPCE(NCAqoi, y.max = 0.19, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)", subtitle = expression(paste(rho, "= 0.1"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nvca_01.pdf", width = 9, height = 3.5)
# PlotAPCE(NVCAqoi, y.max = 0.38, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)", subtitle = expression(paste(rho, "= 0.1")))
# dev.off()
# 
# ## rho = 0.3
# ## Main analysis
# # MCMC
# FTAmcmc <- AiEvalmcmc(FTAdata, rho = 0.3, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NCAmcmc <- AiEvalmcmc(NCAdata, rho = 0.3, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# NVCAmcmc <- AiEvalmcmc(NVCAdata, rho = 0.3, n.mcmc = 60000, verbose = TRUE, out.length = 500)
# 
# # APCE
# FTAace <- CalAPCEparallel(FTAdata, FTAmcmc, subgroup = subg, rho = 0.3, burnin = 2 / 3, size = 5)
# FTAqoi <- APCEsummary(FTAace[["APCE.mcmc"]])
# 
# NCAace <- CalAPCEparallel(NCAdata, NCAmcmc, subgroup = subg, rho = 0.3, burnin = 2 / 3, size = 5)
# NCAqoi <- APCEsummary(NCAace[["APCE.mcmc"]])
# 
# NVCAace <- CalAPCEparallel(NVCAdata, NVCAmcmc, subgroup = subg, rho = 0.3, burnin = 2 / 3, size = 5)
# NVCAqoi <- APCEsummary(NVCAace[["APCE.mcmc"]])
# 
# # Figure S15
# pdf("figs/sens_fta_03.pdf", width = 9, height = 3)
# PlotAPCE(FTAqoi,
#   y.max = 0.16, top.margin = 0.05, bottom.margin = 0.05,
#   label.position = c("top", "bottom", "top", "bottom")
# ) +
#   labs(title = "Failure to Appear (FTA)", subtitle = expression(paste(rho, "= 0.3"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nca_03.pdf", width = 9, height = 3)
# PlotAPCE(NCAqoi, y.max = 0.16, label = FALSE) +
#   labs(title = "New Criminal Activity (NCA)", subtitle = expression(paste(rho, "= 0.3"))) +
#   theme(legend.position = "none")
# dev.off()
# pdf("figs/sens_nvca_03.pdf", width = 9, height = 3.5)
# PlotAPCE(NVCAqoi, y.max = 0.62, label = FALSE) +
#   labs(title = "New Violent Criminal Activity (NVCA)", subtitle = expression(paste(rho, "= 0.3")))
# dev.off()

