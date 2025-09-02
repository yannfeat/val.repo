## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(andrews)

## ----eval=FALSE---------------------------------------------------------------
#  op <- par(mfrow=c(1,2))
#  andrews0(iris, main="andrews0")
#  andrews(iris, main="andrews")
#  par(op)

## ----echo=FALSE---------------------------------------------------------------
op <- par(mfrow=c(1,2))
andrews0(iris, main="andrews0")
andrews(iris, main="andrews")
par(op)

## ----eval=FALSE---------------------------------------------------------------
#  zzz()

## ----eval=FALSE---------------------------------------------------------------
#  andrews (df, type = 1, clr = NULL, step = 100, ymax = 10,          # old parameters
#           alpha = NULL, palcol = NULL, lwd = 1, lty = "solid", ...) # new parameters

## ----eval=FALSE---------------------------------------------------------------
#  op<-par(mfrow=c(1,3))
#  andrews(iris, main="no ymax")
#  andrews(iris, ymax=NA, main="ymax=NA")
#  andrews(iris, ylim=c(-1,3), main="ylim=c(-1,3)")
#  par(op)

## ----echo=FALSE---------------------------------------------------------------
op<-par(mfrow=c(1,3))
andrews(iris, main="no ymax")
andrews(iris, ymax=NA, main="ymax=NA")
andrews(iris, ylim=c(-1,3), main="ylim=c(-1,3)")
par(op)

## -----------------------------------------------------------------------------
andrews(iris, type=3, xlim=c(0,6*pi), ymax=NA)

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, lwd=3)

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=rainbow(nrow(iris)))

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=5) # iris$Species

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=5, palcol=hcl.colors) # iris$Species

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=1) # iris$Sepal.Length

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=1, palcol=function(v) { gray(v) }) # iris$Sepal.Length

## ----eval=FALSE---------------------------------------------------------------
#  andrews(iris, ymax=NA, clr=1, palcol=gray) # iris$Sepal.Length

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, alpha=0.1) 

## -----------------------------------------------------------------------------
andrews(iris, ymax=NA, clr=5, alpha=0.1, lwd=2)

## -----------------------------------------------------------------------------
deftype(1)

## -----------------------------------------------------------------------------
deftype("sine", xlim = c(-pi, pi), function(n, t) {
  n <- as.integer(if (n<1) 1 else n)
  m <- matrix(NA, nrow=length(t), ncol=n)
  for (i in 1:n) m[,i] <- sin(i*t)
  m
})
andrews(iris, "sine", ymax=NA, clr=5)

