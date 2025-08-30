## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup, eval=FALSE--------------------------------------------------------
# library(AdapDiscom)
# library(MASS)

## ----eval=FALSE---------------------------------------------------------------
# # Number of variables
# p <- 10
# 
# # AR(1) structure
# Sigma_ar1 <- generate.cov(p, example = 1)
# print(Sigma_ar1[1:3, 1:3])
# 
# # Block diagonal structure
# Sigma_block <- generate.cov(p, example = 2)
# print(Sigma_block[1:3, 1:3])
# 
# # Kronecker product structure
# Sigma_kron <- generate.cov(p, example = 3)
# print(dim(Sigma_kron))

## ----eval=FALSE---------------------------------------------------------------
# set.seed(123)
# # Set parameters
# p <- 300
# n <- 440
# n.tuning <- 200
# n.test <- 400
# p1 <- p%/%3
# p2 <- p%/%3
# p3 <- p - p1 - p2
# pp <- c(p1, p2, p3)  # Block sizes
# sigma <- 1

## ----eval=FALSE---------------------------------------------------------------
# # Generate different covariance matrices for each block
# cov.mat1 <- generate.cov(p1, 1)  # AR(1) structure for block 1
# cov.mat2 <- generate.cov(p2, 2)  # Block diagonal for block 2
# cov.mat3 <- generate.cov(p3, 3)  # Kronecker product for block 3

## ----eval=FALSE---------------------------------------------------------------
# # Generate true beta coefficients
# beta1 <- rep(c(rep(0.5, 5), rep(0, 95)), p/100)
# beta.true <- beta1
# 
# # Generate complete data for all samples
# pre.x1 <- mvrnorm(n + n.tuning + n.test, rep(0, p%/%3), cov.mat1)
# pre.x2 <- mvrnorm(n + n.tuning + n.test, rep(0, p%/%3), cov.mat2)
# pre.x3 <- mvrnorm(n + n.tuning + n.test, rep(0, p%/%3), cov.mat3)
# pre.x <- cbind(pre.x1, pre.x2, pre.x3)
# pre.ep <- rnorm(n + n.tuning + n.test, 0, sigma)
# 
# # Training data
# n.com <- n/4
# n1 <- n2 <- n3 <- n4 <- n.com
# X.train <- pre.x[1:n, ]
# ep <- pre.ep[1:n]
# y.train <- X.train %*% beta1 + ep
# colnames(X.train) <- paste0("X", 1:p)
# 
# # Introduce missing data pattern
# X.train[(n1 + 1):(n1 + n2), (p1 + p2 + 1):(p1 + p2 + p3)] <- NA
# X.train[(n1 + n2 + 1):(n1 + n2 + n3), (p1 + 1):(p1 + p2)] <- NA
# X.train[(n1 + n2 + n3 + 1):(n1 + n2 + n3 + n4), (1:p1)] <- NA
# 
# # Tuning data
# X.tuning <- pre.x[(n + 1):(n + n.tuning), ]
# ep.tuning <- pre.ep[(n + 1):(n + n.tuning)]
# y.tuning <- X.tuning %*% beta1 + ep.tuning
# colnames(X.tuning) <- paste0("X", 1:p)
# 
# # Test data
# X.test <- pre.x[(n + n.tuning + 1):(n + n.tuning + n.test), ]
# ep.test <- pre.ep[(n + n.tuning + 1):(n + n.tuning + n.test)]
# y.test <- X.test %*% beta1 + ep.test
# colnames(X.test) <- paste0("X", 1:p)

## ----eval=FALSE---------------------------------------------------------------
# # Run AdapDiscom with default parameters
# result <- adapdiscom(
#   beta = beta.true,      # True coefficients (optional, for evaluation)
#   x = X.train,          # Training data
#   y = y.train,          # Training response
#   x.tuning = X.tuning,  # Tuning data
#   y.tuning = y.tuning,  # Tuning response
#   x.test = X.test,      # Test data
#   y.test = y.test,      # Test response
#   nlambda = 20,         # Number of lambda values
#   nalpha = 10,          # Number of alpha values
#   pp = pp,              # Block sizes
#   robust = 0,           # Classical estimation
#   standardize = TRUE,   # Standardize data
#   itcp = TRUE          # Include intercept
# )
# 
# # View results
# print(paste("R-squared:", round(result$R2, 4)))

## ----eval=FALSE---------------------------------------------------------------
# # Optimal parameters
# cat("Optimal lambda:", result$lambda, "\n")
# cat("Optimal alpha:", paste(round(result$alpha, 4), collapse = ", "), "\n")
# 
# # Performance metrics
# cat("Training error:", round(result$train.error, 4), "\n")
# cat("Test error:", round(result$test.error, 4), "\n")
# cat("R-squared:", round(result$R2, 4), "\n")
# 
# # Model selection
# cat("Number of selected variables:", result$select, "\n")
# 
# # If true beta provided, evaluation metrics
# if (!is.null(beta.true)) {
#   cat("Estimation error:", round(result$est.error, 4), "\n")
#   cat("False Positive Rate:", round(result$fpr, 4), "\n")
#   cat("False Negative Rate:", round(result$fnr, 4), "\n")
# }
# 
# # Estimated coefficients
# cat("First 6 estimated coefficients:\n")
# print(round(head(result$a1), 4))

## ----eval=FALSE---------------------------------------------------------------
# # Compare different methods
# methods <- c("adapdiscom", "discom", "fast_adapdiscom", "fast_discom")
# results <- list()
# 
# # AdapDiscom
# results$adapdiscom <- result # Already computed above
# 
# # DISCOM
# results$discom <- discom(
#   beta = beta.true, x = X.train, y = y.train,
#   x.tuning = X.tuning, y.tuning = y.tuning,
#   x.test = X.test, y.test = y.test,
#   nlambda = 20, nalpha = 10, pp = pp
# )
# 
# # Fast AdapDiscom
# results$fast_adapdiscom <- fast_adapdiscom(
#   beta = beta.true, x = X.train, y = y.train,
#   x.tuning = X.tuning, y.tuning = y.tuning,
#   x.test = X.test, y.test = y.test,
#   nlambda = 20, nalpha = 10, pp = pp
# )
# 
# # Fast DISCOM
# results$fast_discom <- fast_discom(
#   beta = beta.true, x = X.train, y = y.train,
#   x.tuning = X.tuning, y.tuning = y.tuning,
#   x.test = X.test, y.test = y.test,
#   nlambda = 20, pp = pp
# )
# 
# # Compare performance
# comparison <- data.frame(
#   Method = names(results),
#   Test_Error = round(sapply(results, function(x) x$test.error), 4),
#   R_Squared = round(sapply(results, function(x) x$R2), 4),
#   Selected_Vars = sapply(results, function(x) x$select),
#   Est_Error = round(sapply(results, function(x) x$est.error), 4),
#   FPR = round(sapply(results, function(x) x$fpr), 4),
#   FNR = round(sapply(results, function(x) x$fnr), 4),
#   Time = round(sapply(results, function(x) x$time), 2)
# )
# 
# print(comparison)

## ----eval=FALSE---------------------------------------------------------------
# # Run with robust estimation
# result_robust <- adapdiscom(
#   beta = beta.true,
#   x = X.train, y = y.train,
#   x.tuning = X.tuning, y.tuning = y.tuning,
#   x.test = X.test, y.test = y.test,
#   nlambda = 20, nalpha = 10, pp = pp,
#   robust = 1,        # Enable robust estimation
#   k.value = 1.5      # Huber tuning parameter
# )

## ----eval=FALSE---------------------------------------------------------------
# # Advanced usage with custom parameters
# result_advanced <- adapdiscom(
#   beta = beta.true,
#   x = X.train, y = y.train,
#   x.tuning = X.tuning, y.tuning = y.tuning,
#   x.test = X.test, y.test = y.test,
#   nlambda = 30,              # More lambda values
#   nalpha = 15,               # More alpha values
#   pp = pp,
#   robust = 0,
#   standardize = TRUE,
#   itcp = TRUE,
#   lambda.min.ratio = 0.001,  # Custom lambda range
#   k.value = 1.5
# )

