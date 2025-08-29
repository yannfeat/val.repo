
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actLifer

<img align="right" width="150" height="175" src="man/figures/hex-actLifer.png">

<!-- badges: start -->

[![R-CMD-check](https://github.com/g-rade/actLifer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/g-rade/actLifer/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/actLifer)](https://CRAN.R-project.org/package=actLifer)
<!-- badges: end -->

The `actLifer` package contains functions that create a life table based
on mortality data, and ultimately calculates life expectancy from data
on annual deaths for given ages/age groups. This package also contains
mortality data from CDC wonder.

This package will be useful for actuaries, epidemiologists, or any data
scientists working with mortality data.

## Installation

You can install the development version of actLifer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("g-rade/actLifer")
```

You can install the reslease version of actLifer from CRAN with:

``` r
#install.packages("actLifer")
```

## How to use actLifer

``` r
library(actLifer)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## Take a look at the mortality2 data fram
head(mortality2)
#> # A tibble: 6 × 3
#>   age_group deaths population
#>   <chr>      <dbl>      <dbl>
#> 1 < 1 year   23161    3970145
#> 2 1 year      1568    3995008
#> 3 2 years     1046    3992154
#> 4 3 years      791    3982074
#> 5 4 years      640    3987656
#> 6 5 years      546    4032515

## Use the lifetable function to make a custom life table with just
## CentralDeathRate, PropToSurvive, and LifeExpectancy by setting includeAllSteps = FALSE

lifetable(mortality2, "age_group", "population", "deaths", FALSE, TRUE, TRUE)
#> # A tibble: 85 × 6
#>    age_group deaths population CentralDeathRate PropToSurvive LifeExpectancy
#>    <chr>      <dbl>      <dbl>            <dbl>         <dbl>          <dbl>
#>  1 < 1 year   23161    3970145         0.00583          1               75.9
#>  2 1 year      1568    3995008         0.000392         0.994           75.3
#>  3 2 years     1046    3992154         0.000262         0.994           74.4
#>  4 3 years      791    3982074         0.000199         0.994           73.4
#>  5 4 years      640    3987656         0.000160         0.993           72.4
#>  6 5 years      546    4032515         0.000135         0.993           71.4
#>  7 6 years      488    4029655         0.000121         0.993           70.4
#>  8 7 years      511    4029991         0.000127         0.993           69.4
#>  9 8 years      483    4159114         0.000116         0.993           68.4
#> 10 9 years      462    4178524         0.000111         0.993           67.4
#> # ℹ 75 more rows


## Or show everything by setting includeAllSteps=TRUE, includeCDR=TRUE, and includePS=TRUE which are the default values
lifetable(mortality2, "age_group", "population", "deaths")
#> # A tibble: 85 × 11
#>    age_group deaths population CentralDeathRate ConditionalProbDeath
#>    <chr>      <dbl>      <dbl>            <dbl>                <dbl>
#>  1 < 1 year   23161    3970145         0.00583              0.00582 
#>  2 1 year      1568    3995008         0.000392             0.000392
#>  3 2 years     1046    3992154         0.000262             0.000262
#>  4 3 years      791    3982074         0.000199             0.000199
#>  5 4 years      640    3987656         0.000160             0.000160
#>  6 5 years      546    4032515         0.000135             0.000135
#>  7 6 years      488    4029655         0.000121             0.000121
#>  8 7 years      511    4029991         0.000127             0.000127
#>  9 8 years      483    4159114         0.000116             0.000116
#> 10 9 years      462    4178524         0.000111             0.000111
#> # ℹ 75 more rows
#> # ℹ 6 more variables: ConditionalProbLife <dbl>, NumberToSurvive <dbl>,
#> #   PropToSurvive <dbl>, PersonYears <dbl>, TotalYears <dbl>,
#> #   LifeExpectancy <dbl>
```

## Contributors

- [Grace Rade](https://github.com/g-rade)
- [Maeve Tyler-Penny](https://github.com/mctp546)
- [Julia Ting](https://github.com/jtingy)
