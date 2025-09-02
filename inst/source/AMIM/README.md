
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AMIM

<!-- badges: start -->
<!-- badges: end -->

The goal of AMIM is to provide an easy function to compute the rolling
window AMIM following the paper of Tran & Leirvik (2019), “A simple but
powerful measure of market efficiency”. Finance Research Letters, 29,
pp.141-151.

## Installation

You can install the released version of AMIM from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("AMIM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AMIM)
library(data.table)

data <- AMIM::exampledata # load the example data

AMIM <- AMIM.roll(data.table = data, identity.col = "ticker", rollWindow = 60, Date.col = "Date", return.col = "RET", min.obs = 30, max.lag = 10)
#>   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   4%  |                                                                              |======                                                                |   8%  |                                                                              |=========                                                             |  12%  |                                                                              |============                                                          |  17%  |                                                                              |===============                                                       |  21%  |                                                                              |==================                                                    |  25%  |                                                                              |====================                                                  |  29%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  38%  |                                                                              |=============================                                         |  42%  |                                                                              |================================                                      |  46%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  54%  |                                                                              |=========================================                             |  58%  |                                                                              |============================================                          |  62%  |                                                                              |===============================================                       |  67%  |                                                                              |==================================================                    |  71%  |                                                                              |====================================================                  |  75%  |                                                                              |=======================================================               |  79%  |                                                                              |==========================================================            |  83%  |                                                                              |=============================================================         |  88%  |                                                                              |================================================================      |  92%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%
AMIM[, .SD[(.N - 5):(.N), ], by = ticker] # show the last 5 observations for each ticker
#>     ticker  N       Date       MIM        CI        AMIM
#>  1:      A  2 2021-07-06 0.7044131 0.7604725 -0.23404162
#>  2:      A  2 2021-07-07 0.7044131 0.7604725 -0.23404162
#>  3:      A  3 2021-07-08 0.8058670 0.8110500 -0.02743054
#>  4:      A  3 2021-07-09 0.8017444 0.8110500 -0.04924920
#>  5:      A  3 2021-07-10 0.8017444 0.8110500 -0.04924920
#>  6:      A  3 2021-07-11 0.8017444 0.8110500 -0.04924920
#>  7:      B NA 2021-07-06        NA        NA          NA
#>  8:      B NA 2021-07-07        NA        NA          NA
#>  9:      B NA 2021-07-08        NA        NA          NA
#> 10:      B NA 2021-07-09        NA        NA          NA
#> 11:      B NA 2021-07-10        NA        NA          NA
#> 12:      B NA 2021-07-11        NA        NA          NA
```
