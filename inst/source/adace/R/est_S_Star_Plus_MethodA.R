#' Estimate the treatment effects for population S_*+ using Method A
#'
#' @description
#'  The est_S_Star_Plus_MethodA function produces estimation of treatment
#'  effects for the population that can adhere to the experimental
#'  treatment (S_*+). This method (Method A) is based on the potential outcome
#'  under the hypothetical alternative treatment .
#'
#' @param X Matrix of baseline variables. Each row contains the baseline values
#' for each patient.
#' @param A Matrix of indicator for adherence. Each row  of A contains the
#' adherence information for each patient. Each column contains the adherence
#' indicator after each intermediate time point. A = 1 means adherence
#' and A=0 means non-adherence. Monotone missing is assumed.
#' @param Z List of matrices. Intermediate efficacy and safety outcomes that
#' can affect the probability of adherence. For each matrix, the structure
#' is the same as variable X.
#' @param Y Numeric vector of the final outcome (E.g., primary endpoint).
#' @param TRT Numeric vector of treatment assignment. TRT=0 for the control
#' group and TRT =1 for the experimental treatment group.
#'
#' @return A list containing the following components:
#'   \item{trt_diff}{Estimate of treatment difference for S_{++} using Method A}
#'   \item{se}{Estimated standard error}
#'   \item{res1}{Estimated mean for the treatment group}
#'   \item{res0}{Estimated mean for the control group}
#'   \item{se_res1}{Estimated standard error for the treatment group}
#'   \item{se_res0}{Estimated standard error for the control group}
#'
#'#' @details
#' The average treatment difference can be denoted as
#'
#' \deqn{latex}{\mu_{d,*+} = E\{Y(1)-Y(0)|A(1) = 1\}}
#'
#' The method A exploits the joint distribution of X, Z, and Y by creating a
#' "virtual twin" of the patient from the assigned treatment and estimate the
#' potential outcome of that patient on the alternative treatment for
#' comparison. The variance estimation for the treatment effect is
#' constructed using the sandwich method.
#' Details can be found in the references.
#'
#' The intermediate post-baseline measurements for each intermediate time point
#' are estimated by regressing Z on X using subjects with experimental treatment
#' or placebo. The covariance matrix is estimated based
#' on the residuals of the regression.
#'
#' The probability of adherence is estimated by
#' regressing A on X, Z by using all data. The logistic regression
#' is used in this function.
#'
#' The expected treatment effect is estimated by
#' regression Y on X, Z using subjects with experimental treatment or placebo.
#'
#' The indicator of adherence prior to the first intermediate time point is not
#' included in this model since this function assumes no intercurrent events
#' prior to the first time point. Thus,  the first element of Z should not have
#' missing values.
#'
#' Each element of Z contains the variables at each intermediate time point,
#' i.e., the first element of Z contains the intermediate variables at time
#' point 1, the second element contains the intermediate variables at time point
#'  2, etc.
#'
#' @references
#' Qu, Yongming, et al. "A general framework for treatment effect
#' estimators considering patient adherence."
#' Statistics in Biopharmaceutical Research 12.1 (2020): 1-18.
#'
#' Zhang, Ying, et al. "Statistical inference on the estimators of the adherer
#' average causal effect."
#' Statistics in Biopharmaceutical Research (2021): 1-4.
#'
#'
#' @examples
#'  library(MASS)
#'  j<- 500
#'  p_z <- 6 ## dimension of Z at each time point
#'  n_t <- 4 ## number of time points
#'  alphas <- list()
#'  gammas <- list()
#'  z_para <- c(-1/p_z, -1/p_z, -1/p_z, -1/p_z, -0.5/p_z,-0.5/p_z, -0.5/p_z,
#'  -0.5/p_z)
#'  Z <- list()
#'  beta = c(0.2, -0.3, -0.01, 0.02, 0.03, 0.04, rep(rep(0.02,p_z), n_t))
#'  beta_T = -0.2
#'  sd_z_x = 0.4
#'  X = mvrnorm(j, mu=c(1,5,6,7,8), Sigma=diag(1,5))
#'  TRT = rbinom(j, size = 1,  prob = 0.5)
#'  Y_constant <- beta[1]+(X%*%beta[2:6])
#'  Y0 <- 0
#'  Y1 <- 0
#'  A <- A1 <- A0 <- matrix(NA, nrow = j, ncol = n_t)
#'  gamma <- c(1,-.1,-0.05,0.05,0.05,.05)
#'  A0[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
#'  (X %*% gamma[2:6])))))
#'  A1[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
#'  (X %*% gamma[2:6])))))
#'  A[,1] <- A1[,1]*TRT + A0[,1]*(1-TRT)

#'  for(i in 2:n_t){
#'    alphas[[i]] <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04, -0.4),
#'    p_z),ncol=p_z)
#'    gammas[[i]] <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(z_para[i],p_z))
#'    Z0 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,]) + mvrnorm(j, mu = rep(0,p_z)
#'    , Sigma = diag(sd_z_x,p_z))
#'    Z1 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,])+alphas[[i]][7,] +
#'      mvrnorm(j, mu = rep(0,p_z), Sigma = diag(sd_z_x,p_z))
#'    Z[[i]] <- Z1*TRT + Z0*(1-TRT)
#'    Y0 <- (Y0 + Z0 %*% matrix(beta[ (7 + (i-1)*p_z):
#'    (6+p_z*i)],ncol = 1) )[,1]
#'    Y1 <- (Y1 + Z1 %*% matrix(beta[ (7 + (i-1)*p_z):
#'    (6+p_z*i)],ncol = 1) )[,1]
#'    A0[,i] <- rbinom(j, size = 1,
#'                     prob = 1/(1+exp(-(gammas[[i]][1]+
#'                     (X%*%gammas[[i]][2:6])+Z0%*%matrix(gammas[[i]][7:
#'                     (7+p_z-1)], ncol=1))[,1])))*A0[,i-1]
#'    A1[,i] <- rbinom(j, size = 1,
#'                     prob = 1/(1+exp(-(gammas[[i]][1]+
#'                     (X%*%gammas[[i]][2:6])+Z1%*%matrix(gammas[[i]][7:
#'                     (7+p_z-1)], ncol=1))[,1])))*A1[,i-1]
#'
#'    A[,i] <- A1[,i]*TRT + A0[,i]*(1-TRT)
#'  }
#'  Y0 <- Y0 + rnorm(j, mean = 0, sd = 0.3) + Y_constant
#'  Y1 <- Y1 + + beta_T + rnorm(j, mean = 0, sd = 0.3) + Y_constant

#'  Y <- as.vector( Y1*TRT+Y0*(1-TRT))

#'  for(i in 2:n_t){
#'    Z[[i]][A[,(i-1)]==0,] <- NA
#'  }

#'  Z[[1]] <- matrix(NA, nrow=nrow(Z1),ncol=ncol(Z1))

#'  Y[A[,n_t] == 0] <- NA
#'  # estimate the treatment difference
#'  fit <- est_S_Star_Plus_MethodA(X, A, Z, Y, TRT)
#'  fit
#'  # Calculate the true values
#'  true1 <- mean(Y1[A1[,n_t]==1])
#'  true1
#'  true0 <- mean(Y0[A1[,n_t]==1])
#'  true0
#'  true_d  =  true1 - true0
#'  true_d
#'
#' @export
est_S_Star_Plus_MethodA <- function(X, A, Z, Y, TRT) { # nolint
  res1 <- res0 <- res <- NULL
  se1 <- se0 <- se <- NULL
  Y <- as.numeric(Y)  # nolint
  TRT <- as.numeric(TRT) # nolint
  X <- matrix(X, nrow = length(Y))  # nolint
  A <- matrix(A, nrow = length(Y)) # nolint
  # Standardize X
  #X <- apply(X,2,function(x) {(x-mean(x))/sd(x)})  # nolint
  n <- dim(X)[1]
  n_time_points <- length(Z)
  Z <- lapply(Z, as.matrix)  # nolint
  # Provide column names for X, Z, A
  x_col_names <- paste("X_", 1:dim(X)[2], sep = "")
  a_col_names <- paste("A_", 1:dim(A)[2], sep = "")
  colnames(X) <- x_col_names
  colnames(A) <- a_col_names
  for (i in 1:n_time_points) {
    colnames(Z[[i]]) <- paste("Z_", i, "_", 1:dim(Z[[i]])[2], sep = "")
  }
  z_cbind <- do.call(cbind, Z[-1])
  data <- data.frame(X, TRT, z_cbind, Y, A)

  models_z_x <- list()
  cov_z_x <- list()

  for (i in 2:n_time_points) {
    Z_id <- paste("Z_", i, sep = "")  # nolint
    form <- paste("cbind(", paste(colnames(z_cbind)[grep(Z_id,
                  colnames(z_cbind))], collapse = ","), ")~",
                  paste(c(x_col_names, "TRT"), collapse = "+"), sep = "")
    models_z_x[[i]] <- lm(form, data = data.frame(data))
    if (dim(Z[[i]])[2] > 1) {
      cov_z_x[[i]] <- cov(models_z_x[[i]]$residuals)
    } else {
      cov_z_x[[i]] <- matrix(var(models_z_x[[i]]$residuals), 1, 1)
    }
  }
  # fit parametric model for Y
  model_y0_x_z0 <- paste("Y~", paste("X_", 1:dim(X)[2], sep = "",
                        collapse = "+"), "+", paste(colnames(z_cbind), sep = "",
                        collapse = "+"), sep = "")
  fit_y0_x_z0 <- lm(model_y0_x_z0,
                    data = data[TRT == 0 & A[, n_time_points] == 1, ])
  psi0_x_z0 <- predict(fit_y0_x_z0, newdata = data)
  beta0_hat <- c(fit_y0_x_z0$coef)
  # Predict Z using models.Z.X
  Zs0_pred <- list()  # nolint
  for (i in 2:n_time_points) {
    Zs0_pred[[i]] <- predict(models_z_x[[i]],
                             newdata = data.frame(X, TRT = rep(0, nrow(X))))
    if (!is.matrix(Zs0_pred[[i]])) {
      temp <- matrix(Zs0_pred[[i]], ncol = 1)
      colnames(temp) <- colnames(Z[[i]])
      Zs0_pred[[i]] <- temp
    }
  }
  phi0_x <- predict(fit_y0_x_z0,
                    newdata = data.frame(X, do.call(cbind, Zs0_pred)))
  Y_clean <- NA_replace(Y)             # nolint
  res <- sum(TRT * A[, n_time_points] * Y_clean - TRT * A[, n_time_points] *
               phi0_x) / sum(TRT * A[, n_time_points])

  g1 <- (1 - TRT) * A[, n_time_points] * (Y - psi0_x_z0) *
    cbind(rep(1, n), X, z_cbind)
  g1[A[, n_time_points] == 0, ] <- 0
  covariate_long <- vector("list",n_time_points)
  covariate_long[2:n_time_points] <- lapply(models_z_x[-1], model.matrix.lm)
  g2 <- list()
  for (i in 2:n_time_points) {
    g2[[i]] <- array(apply(models_z_x[[i]]$model[[1]] -
                             predict(models_z_x[[i]]), 2,
                           function(x) {covariate_long[[i]] * x}), # nolint
                     dim = c(dim(covariate_long[[i]]),
                             dim(models_z_x[[i]]$model[[1]])[2]))
  }

  g4 <- TRT * A[, n_time_points] * (Y_clean - phi0_x)
  g5 <- TRT * A[, n_time_points]

  # Estimating expection of deriatives
  partial_g4_beta <- -c(mean(TRT * A[, n_time_points]),
                        colMeans(TRT * A[, n_time_points] * X),
                        colMeans(TRT * A[, n_time_points] *
                                   do.call(cbind, Zs0_pred)))
  partial_g4_alpha <- list()
  for (i in 2:n_time_points) {
    partial_g4_alpha_z <- matrix(-beta0_hat[paste("Z_", i, "_",
                          1:dim(Z[[i]])[2], sep = "")], ncol = 1) *
      mean(TRT * A[, n_time_points])
    partial_g4_alpha_zx <- matrix(-beta0_hat[paste("Z_", i, "_",
                          1:dim(Z[[i]])[2], sep = "")], ncol = 1) %*%
      colMeans(TRT * A[, n_time_points] * X)
    partial_g4_alpha_zt <- matrix(0, nrow = dim(Z[[i]])[2], ncol = 1)
    partial_g4_alpha[[i]] <- cbind(partial_g4_alpha_z,
                                   partial_g4_alpha_zx,
                                   partial_g4_alpha_zt)
  }
  covariate <- cbind(rep(1, n), X, z_cbind)
  covariate_T0A0 <- covariate[TRT == 0 & A[, n_time_points] == 1, ]   # nolint

  partial_g1_beta <- -t(covariate_T0A0) %*% covariate_T0A0 / n

  partial_g2_alpha <- list()
  for (i in 2:n_time_points) {
    partial_g2_alpha[[i]] <- -t(covariate_long[[i]]) %*% covariate_long[[i]] / n
  }

  tau_est <- res
  se_main <- -g1 %*% inv_svd(partial_g1_beta)  %*% partial_g4_beta / mean(g5)
  se_g2 <- 0
  for (i in 2:n_time_points) {
    non_miss <- rowSums(is.na(Z[[i]])) == 0
    for (j in 1:dim(g2[[i]])[3]) {
      temp2 <- matrix(0, nrow = n, ncol = ncol(g2[[i]]))
      temp2[non_miss, ] <- g2[[i]][, , j]
      se_g2 <- se_g2 - temp2 %*% solve(partial_g2_alpha[[i]]) %*%
        partial_g4_alpha[[i]][j, ] / mean(g5)
    }
  }
  se <- sd(as.numeric(se_main + se_g2) + as.numeric(1 / mean(g5) *
        (g4 - tau_est * g5))) / sqrt(n)
  res1 <- sum(TRT * A[, n_time_points] * Y_clean) /
    sum(TRT * A[, n_time_points])
  res0 <- sum(TRT * A[, n_time_points] * phi0_x) / sum(TRT * A[, n_time_points])
  se1 <- sd(1 / mean(g5) * (TRT * A[, n_time_points] *
                              Y_clean - res1 * g5)) / sqrt(n)
  se0 <- sd(as.numeric(se_main + se_g2) + as.numeric(1 / mean(g5) *
          (TRT * A[, n_time_points] * phi0_x - res0 * g5))) / sqrt(n)
  rval <- list(trt_diff = res,
             se = se,
             res1 = res1,
             res0 = res0,
             se_res1 = se1,
             se_res0 = se0
  )
  return(rval)
}
