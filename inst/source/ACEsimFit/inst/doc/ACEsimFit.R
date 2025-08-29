## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ACEsimFit)

## ----simulation---------------------------------------------------------------
kindata <- kinsim_double(
    GroupNames = c("SStwins", "OStwins"),
    GroupSizes = c(120, 60),
    GroupRel = c(.75, 0.5),
    GroupR_c = c(1, 1),
    mu = c(0, 0),
    ace1 = c(.6, .2, .2),
    ace2 = c(.6, .2, .2),
    ifComb = TRUE
)
head(kindata)

## ----Sim_Fit------------------------------------------------------------------
time1 <- Sys.time()
results_fit <- Sim_Fit(
  GroupNames = c("SStwins", "OStwins"),
  GroupSizes = c(120, 60),
  nIter = 50,
  SSeed = 62,
  GroupRel = c(.75, 0.5),
  GroupR_c = c(1, 1),
  mu = c(0, 0),
  ace1 = c(.6, .2, .2),
  ace2 = c(.6, .2, .2),
  ifComb = TRUE,
  lbound = FALSE,
  saveRaw = FALSE
)
time2 <- Sys.time()
## FYI, the time used for the results above is here. So design your simulation wisely!!!
time2 - time1

## ----resultsDemo--------------------------------------------------------------
results_fit[["Iteration1"]][["Results"]][["nest"]]

## ----powerCalculation---------------------------------------------------------
N <- 180 ##the total number of kin pairs you used in your previous simulation
## Calculate the average diffLL between ACE and CE model.
DiffLL <- numeric()
for(i in 1:50){
  DiffLL[i] <- results_fit[[1]][["Results"]][["nest"]]$diffLL[3] 
}
meanDiffLL <- mean(DiffLL)
## Calculate the power based on an alpha level of .05
Power <- 1- pchisq(qchisq(1-.05, 1), 1, meanDiffLL)
Power

## ----powerCalculation2--------------------------------------------------------
Power_LS(N1=120, N2=60, h2=.6, c2=.2, R1 = .75, R2 = 0.5, alpha = 0.05)

