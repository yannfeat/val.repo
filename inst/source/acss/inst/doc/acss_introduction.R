## ----load---------------------------------------------------------------------
library("acss")

## ----acss_1-------------------------------------------------------------------
acss(c("aba", "aaa"))

## ----acss_2-------------------------------------------------------------------

acss(c("01011100", "00030101"), alphabet = c(2, 4))

## ----local_1------------------------------------------------------------------
local_complexity("aaabcbad", alphabet=4, span=6)

## ----Bayesian_1---------------------------------------------------------------
likelihood_d(c("HTHTHTHT","HTHHTHTT"), alphabet=2)

## ----Bayesian_2---------------------------------------------------------------
likelihood_ratio(c("HTHTHTHT", "HTHHTHTT"), alphabet = 2)
prob_random(c("HTHTHTHT", "HTHHTHTT"), alphabet = 2)

## ----span_1-------------------------------------------------------------------
sapply(local_complexity("XXXXXXXOOOOXXXOOOOOOO", alphabet = 2, span = 11), mean)

sapply(local_complexity("XXXXXXXOOOOXXXOOOOOOO", alphabet = 2, 5), mean)

