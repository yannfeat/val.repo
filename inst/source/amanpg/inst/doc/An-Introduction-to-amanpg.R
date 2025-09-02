## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, python.reticulate=FALSE)

## ----tidy=FALSE, eval=FALSE, highlight=FALSE----------------------------------
#  Input initial point (A0,B0) and necessary parameters for the required problem
#  
#  for i=0,1,... do
#    Solve the first subproblem for Da
#    Set alpha = 1
#  
#    while F(Retr(alpha * Da),B) > F(A,B) - alpha * norm(Da)^2 / (2 * t1) do
#      alpha = gamma * alpha
#    end while
#  
#    Set A = Retr(alpha * Da)
#  
#    Solve the second subproblem for Db
#    Set alpha = 1
#  
#    while F(A,Retr(alpha * Db)) > F(A,B) - alpha * norm(Db)^2 / (2 * t2) do
#      alpha = gamma * alpha
#    end while
#  
#    Set B = Retr(alpha * Db)
#  end for
#  
#  Return A as the scores and B as the sparse loadings

## ----eval=FALSE, highlight=TRUE-----------------------------------------------
#  install.packages("amanpg")

## ----eval=FALSE, highlight=TRUE-----------------------------------------------
#  spca.amanpg(z, lambda1, lambda2,
#  			f_palm = 1e5, x0 = NULL, y0 = NULL, k = 0, type = 0,
#  			gamma = 0.5, maxiter = 1e4, tol = 1e-5,
#  			normalize = TRUE, verbose = FALSE)

## -----------------------------------------------------------------------------
library(amanpg)

## -----------------------------------------------------------------------------
# parameter initialization
k <- 4
n <- 1000
p <- 500
lambda1 <- matrix(data=0.1, nrow=k, ncol=1)
lambda2 <- 1

## -----------------------------------------------------------------------------
# data matrix generation
set.seed(10)
z <- matrix(rnorm(n * p), n, p)

# only show a subset of the data matrix for brevity
knitr::kable(as.data.frame(z)[1:10,1:4])

## -----------------------------------------------------------------------------
# see the effects of normalize()
knitr::kable(as.data.frame(normalize(z))[1:10,1:4])

## -----------------------------------------------------------------------------
# function call
fin_sprout <- spca.amanpg(z, lambda1, lambda2, k=4)
print(paste(fin_sprout$iter, "iterations,", fin_sprout$sparsity, "sparsity,", fin_sprout$time))

## -----------------------------------------------------------------------------
# View loadings. Only first 10 rows for brevity
knitr::kable(as.data.frame(fin_sprout$loadings)[1:10,])

## ----fig.align='center', fig.cap="Scree plots for the finite lambda case."----
pr.var <- (apply(fin_sprout$x, 2, sd))^2
pve <- pr.var / sum(pr.var)

par(mfrow=c(1,2))
plot(pve, 
     xlab="Sparse PC", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1), 
     type="b")
plot(cumsum(pve), 
     xlab="Sparse PC", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),
     type="b")

## ----fig.align='center', fig.cap="Biplot for the finite lambda case."---------
y_sub = apply(fin_sprout$loadings, 1, function(row) all(row != 0))
loadings = fin_sprout$loadings[y_sub, ]

par(mfrow=c(1,1))
biplot(fin_sprout$x, loadings, xlab="PC 1", ylab="PC 2")

## -----------------------------------------------------------------------------
# infinite lambda2
inf_sprout <- spca.amanpg(z, lambda1, lambda2=Inf, k=4) 
print(paste(inf_sprout$iter, "iterations,", inf_sprout$sparsity, "sparsity,", inf_sprout$time))

# extract loadings. Only first 10 rows for brevity
knitr::kable(as.data.frame(inf_sprout$loadings)[1:10,])

## ----fig.align='center', fig.cap="Scree plot for lambda=inf case."------------
pr.var <- (apply(inf_sprout$x, 2, sd))^2
pve <- pr.var / sum(pr.var)

par(mfrow=c(1,2))
plot(pve, 
     xlab="Sparse PC", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1), 
     type="b")
plot(cumsum(pve), 
     xlab="Sparse PC", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),
     type="b")

## ----fig.align='center', fig.cap="Biplot for lambda=inf case. Observe that with lower sparsity in the loadings and high-dimensional data, the biplot becomes less readable."----
y_sub = apply(inf_sprout$loadings, 1, function(row) all(row != 0))
loadings = inf_sprout$loadings[y_sub, ]

par(mfrow=c(1,1))
biplot(inf_sprout$x, loadings, xlab="PC 1", ylab="PC 2")

