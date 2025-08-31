## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(adplots)

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2025)
X<-matrix(rnorm(100, mean = 2 , sd = 5))
adplot(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60")

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2025)
X<-matrix(rbeta(100, shape1 = 10, shape2 = 2))
adplot(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60")

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2025)
X<-matrix(rf(100, df1 = 10, df2 = 5))
adplot(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60")

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2030)
X<-matrix(rnorm(30, mean = 2, sd = 5))
udplot(X, npdf = FALSE, lcol = "black", rcol = "grey60", pdfcol = "red")

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2030)
X<-matrix(rnorm(30, mean = 2, sd = 5))
udplot(X, npdf = TRUE, lcol = "black", rcol = "grey60", pdfcol = "red")

## ----eval=TRUE, fig.width=7.18, fig.height=4.5--------------------------------
set.seed(2030)
X<-matrix(rnorm(2025, mean = 2, sd = 5))
udplot(X, npdf = TRUE, lcol = "black", rcol = "grey60", pdfcol = "red")

