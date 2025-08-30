#' Estimate the treatment effects for population S_++ using Method B
#'
#' @description
#'  The est_S_Plus_Plus_MethodB function produces estimation of treatment
#'  effects for the population that can adhere to both treatments (S_++).
#'  This method (Method B) is based on the inverse probability weighting (IPW)
#'  to estimate the treatment difference in a targeted population.
#'
#' @param X Matrix of baseline variables. Each row contains the baseline values
#' for each patient.
#' @param A Matrix of indicator for adherence. Each row  of A contains the
#' adherence information for each patient across multiple time points.
#' Each column contains the adherence indicator after each intermediate time
#' point.
#'  A = 1 means adherence
#' and A=0 means non-adherence. Monotone missing is assumed.
#' @param Z List of matrices. Intermediate efficacy and safety outcomes that can
#'  affect the probability of adherence. For each matrix, the structure is the
#' same as variable X.
#' @param Y Numeric vector of the final outcome (E.g., primary endpoint).
#' @param TRT Numeric vector of treatment assignment. TRT=0 for the control
#' group and TRT =1 for the experimental treatment group.
#'
#' @return A list containing the following components:
#'   \item{trt_diff}{Estimate of treatment difference for S_{++} using Method B}
#'   \item{se}{Estimated standard error}
#'   \item{res1}{Estimated mean for the treatment group}
#'   \item{res0}{Estimated mean for the control group}
#'   \item{se_res1}{Estimated standard error for the treatment group}
#'   \item{se_res0}{Estimated standard error for the control group}
#'
#' @details
#' The average treatment difference can be denoted as
#'
#' \deqn{latex}{\mu_{d,++} = E\{Y(1)-Y(0)|A(0) = 1, A(1) = 1\}}
#'
#' The method B exploits the joint distribution of X, Z, and A to estimate the
#' probability that a patient would adhere to the hypothetical
#' alternative treatment, and then use IPW to estimate treatment different for
#' a given population. The variance estimation for the treatment
#' effect is constructed using the sandwich method. Details can be found
#' in the references.
#'
#' The intermediate post-baseline measurements for each intermediate time point
#' are estimated by regressing Z on X
#' using subjects with experimental treatment or placebo. The covariance matrix
#' is estimated based on the residuals of the regression.
#'
#' The probability of adherence is estimated by
#' regressing A on X, Z by using all data. The logistic regression is used
#' in this function.
#'
#' The indicator of adherence prior to the first intermediate time point is not
#' included in this model since this function assumes no intercurrent events
#' prior to the first time point. Thus, the first element of Z should not have
#' missing values.
#'
#' Each element of Z contains the variables at each intermediate time point,
#' i.e., the first element of Z contains the intermediate variables at time
#' point 1, the second element contains the intermediate variables at time point
#'  2, etc.
#'
#'
#' @references
#' Qu, Yongming, et al. "A general framework for treatment effect estimators
#' considering patient adherence."
#' Statistics in Biopharmaceutical Research 12.1 (2020): 1-18.
#'
#' Zhang, Ying, et al. "Statistical inference on the estimators of the adherer
#' average causal effect."
#' Statistics in Biopharmaceutical Research (2021): 1-4.
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
#'  fit <- est_S_Plus_Plus_MethodB(X, A, Z, Y, TRT)
#'  fit
#'  # Calculate the true values
#'  true1 <- mean(Y1[A1[,n_t]==1 &A0[,n_t]==1])
#'  true1
#'  true0 <- mean(Y0[A1[,n_t]==1 &A0[,n_t]==1])
#'  true0
#'  true_d  =  true1 - true0
#'  true_d
#' @export

est_S_Plus_Plus_MethodB <- function(X, A, Z, Y, TRT) { # nolint
  Y <- as.numeric(Y) # nolint
  TRT <- as.numeric(TRT) # nolint
  Y[is.na(Y)] <- 0
  X <- matrix(X, nrow = length(Y)) # nolint
  A <- matrix(A, nrow = length(Y)) # nolint
  Z <- lapply(Z, as.matrix) # nolint
  n_time_points <- length(Z)
  n <- nrow(X)
  Z_mat <- matrix(unlist(Z[-1]), nrow = dim(X)[1]) # nolint

  # Provide column names for X, Z, A
  X_col_names <- paste("X_", 1:dim(X)[2], sep = "") # nolint
  A_col_names <- paste("A_", 1:dim(A)[2], sep = "") # nolint
  colnames(X) <- X_col_names
  colnames(A) <- A_col_names
  Z_col_names <- list()  # nolint
  for (i in 2:n_time_points) {
    part1 <- paste("Z_", i, sep = "")
    Z_col_names[[i]] <- paste(part1, 1:dim(Z[[i]])[2], sep = "")
  }
  colnames(Z_mat) <- unlist(Z_col_names[-1])
  data <- data.frame(X, TRT, Z_mat, Y, A)

  # Model adherence given X, Z using a logistic model
  form1 <- formula(paste("A_1 ~ ", paste(c(X_col_names,
                                          Z_col_names[[1]]), collapse = " + ")))
  models_A_XZ <- list() # nolint
  models_A_XZ[[1]] <- glm(form1, family = "binomial", data = data) # nolint
  for (i in 2:n_time_points) {

    form <- as.formula(paste(A_col_names[i],
                             paste(c(X_col_names, Z_col_names[[i]]),
                                   collapse = "+"), sep = " ~ "))
    models_A_XZ[[i]] <- glm(form, family = "binomial",
                            data = data[A[, (i - 1)] == 1, ])
  }
  coefs_A_XZ <- list()  # nolint
  preds_A_XZ <- list()  # nolint
  for (i in 1:n_time_points) {
    coefs_A_XZ[[i]] <- c(models_A_XZ[[i]]$coef)
    preds_A_XZ[[i]] <- predict(models_A_XZ[[i]],
                               newdata = data, type = "response")
  }

  # Model Z given X and estimate variance-covariance of Z given X
  models_Z_X <- list()                                            # nolint
  sigma_mats_Z <- list()                                          # nolint
  for (i in 2:n_time_points) {
    models_Z_X[[i]] <- lm(Z_mat[, Z_col_names[[i]]] ~ X + TRT)
    if (dim(Z[[i]])[2] > 1) {
      sigma_mats_Z[[i]] <- cov(models_Z_X[[i]]$residuals)
    } else {
      sigma_mats_Z[[i]] <- matrix(var(models_Z_X[[i]]$residuals),
                                  1, 1)
    }
  }

  # Predict Z using models.Z.X
  Z1s_pred <- list() # nolint
  for (i in 2:n_time_points) {
    Z1s_pred[[i]] <- predict(models_Z_X[[i]],
                             newdata = data.frame(X,
                                                  TRT = rep(1, n)))
  }

  # Estimate alpha of Z_i given T for 1 (intercept) and Xs
  coefs_Z1_1X <- list() # nolint
  for (i in 2:n_time_points) {
    coef_raw <- matrix(coef(models_Z_X[[i]]), ncol = dim(Z[[i]])[2])
    coefs_Z1_1X[[i]] <- matrix(coef_raw[1:(dim(X)[2] + 1), ],
                               ncol = dim(Z[[i]])[2])
    coefs_Z1_1X[[i]][1, ] <- coefs_Z1_1X[[i]][1, ] + coef_raw[nrow(coef_raw), ]
  }

  # Calculate required integrations and assigning names for each column
  Expect_res <- apply(X, 1, Expect_function1D_BU, n_time_points = n_time_points,
                      gammas = coefs_A_XZ, alphas = coefs_Z1_1X,
                      Sigmas = sigma_mats_Z)
  Expect_res_t <- t(Expect_res) # nolint
  names_vec <- NULL
  for (i in 2:n_time_points) {
    z_dim <- dim(Z[[i]])[2]
    names_prob <- paste("prob", i, sep = "")
    names_expa <- paste("expa", i, sep = "")
    names_other <- paste(c("expz", "expaz"), i, sep = "")
    names_expz <- c(paste(names_other[1], 1:z_dim, sep = ""))
    names_expaz <- c(paste(names_other[2], 1:z_dim, sep = ""))
    names_vec <- c(names_vec, names_prob, names_expz,
                   names_expa, names_expaz)
  }
  colnames(Expect_res_t) <- names_vec

  prob1_expa1 <- cbind(preds_A_XZ[[1]],preds_A_XZ[[1]]*(1-preds_A_XZ[[1]]))
  colnames(prob1_expa1) <- c("prob1","expa1")
  Expect_res_t <- cbind(Expect_res_t,prob1_expa1)

  names_vec <- c(names_vec, c("prob1","expa1"))

  # Calculate the probability of adherence at the end of study
  probs_vec <- paste("prob", 1:n_time_points, sep = "")
  Expect_probs <- Expect_res_t[, probs_vec]   # nolint
  Expect_A_X <- 1 # nolint
  for (i in 1:n_time_points) {
    Expect_A_X <- Expect_A_X * Expect_probs[, i]   # nolint
  }

  # Calculate several integrals
  Expect_AZ_X <- list()  # nolint
  Expect_AA_X <- list()  # nolint
  Expect_AA_Z_X <- list()  # nolint
  for (i in 2:n_time_points) {
    names_target <- paste(c("expz", "expa", "expaz"), i, sep = "")
    prob_remove <- paste("prob", i, sep = "")
    Expect_AZ_X[[i]] <- Expect_A_X *
      Expect_res_t[, grepl(names_target[1], names_vec)] /
      Expect_res_t[, prob_remove]
    Expect_AA_Z_X[[i]] <-  Expect_A_X *
      Expect_res_t[, grepl(names_target[3], names_vec)] /
      Expect_res_t[, prob_remove]
  }

  for (i in 1:n_time_points) {
    names_target <- paste(c("expz", "expa", "expaz"), i, sep = "")
    prob_remove <- paste("prob", i, sep = "")
    Expect_AA_X[[i]] <- Expect_A_X *
      Expect_res_t[, grepl(names_target[2], names_vec)] /
      Expect_res_t[, prob_remove]
  }

  # Calcuate a point estimation for S_tar_plus
  # Calcuate estimator for second term
  res0 <- sum((1 - TRT) * A[, n_time_points] * Expect_A_X * Y) /
    sum((1 - TRT) * A[, n_time_points] * Expect_A_X)

  # Prepare for plug-in variance estimator
  # g1,g2,g3, g4-res1*g5 are estimating equations
  # Prepare for the plug-in variance estimator
  # Calcuate sample values of estimation equations

  # At each time point, for each dimension of Z, we have an estimation equation
  # g2 = X(Z_i - X\alpha), thus there will be layers of for loop.
  covariate_long <- vector("list",n_time_points)
  covariate_long[2:n_time_points] <- lapply(models_Z_X[-1], model.matrix.lm)
  g2 <- list()
  for (i in 2:n_time_points) {
    g2_main <- (models_Z_X[[i]]$model$`Z_mat[, Z_col_names[[i]]]`
                 - predict(models_Z_X[[i]]))
    if (dim(Z[[i]])[2] > 1) {
      dim_z <- dim(g2_main)[2]
      res <- list()
      for (j in 1:dim_z) {
        res[[j]] <- g2_main[, j] * covariate_long[[i]]
      }
      g2[[i]] <- res
    }
    else {
      g2[[i]] <- g2_main * covariate_long[[i]]
    }
  }

  # At each time point, g3 = (1, x, z)^T(A - Pr(A=1|x, z, \gamma))
  preds_A_XZ_clean <- lapply(preds_A_XZ, NA_replace)  # nolint
  g3 <- list()
  for (i in 1:n_time_points) {
    if (i == 1)
      g3[[i]] <- (A[, i] - preds_A_XZ_clean[[i]]) * cbind(rep(1, n), X)
    else {
      g3[[i]] <- A[, (i - 1)] * (A[, i] - preds_A_XZ_clean[[i]]) *
        cbind(rep(1, n), X, Z[[i]])
      g3[[i]][A[, (i - 1)] == 0, ] <- 0
    }

  }

  g4 <- (1 - TRT) * A[, n_time_points] * Expect_A_X * Y
  g5 <- (1 - TRT) * A[, n_time_points] * Expect_A_X


  # Estimating expection of deriatives using sample averages
  partial_g4_alpha <- list()
  part1_partial <- (1 - TRT) * A[, n_time_points] * Y
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    partial_vec <- matrix(part1_partial * (Expect_AZ_X[[i]] - Expect_A_X *
                                             Z1s_pred[[i]]), ncol = dim_z)
    partial_g4_alpha_1 <-  apply(partial_vec, 2, mean)
    partial_g4_alpha_x <- matrix(rep(NA, dim_z * dim(X)[2]), ncol = dim_z)
    for (j in 1:dim_z) {
      partial_g4_alpha_x[, j] <- apply(t(X) %*%
                                diag(as.vector(partial_vec[, j])), 1, mean)
    }
    partial_g4_alpha[[i]] <- solve(sigma_mats_Z[[i]]) %*%
      t(rbind(partial_g4_alpha_1, partial_g4_alpha_x, partial_g4_alpha_1))
  }

  partial_g4_gamma <- list()
  for (i in 1:n_time_points) {
    if (i == 1) {
      constant <- (1 - TRT) * A[, n_time_points] * Y
      vec1 <- Expect_AA_X[[i]]
      partial_g4_gamma[[i]] <-  c(mean(constant * vec1),
                                  apply(t(X) %*% diag(as.vector(constant *
                                                                  vec1)), 1,
                                        mean))
    } else {
      constant <- (1 - TRT) * A[, n_time_points] * Y
      vec1 <- Expect_AA_X[[i]]
      vec3 <- Expect_AA_Z_X[[i]]
      partial_g4_gamma[[i]] <-  c(mean(constant * vec1),
                  apply(t(X) %*% diag(as.vector(constant * vec1)), 1, mean),
                  apply(matrix(constant * vec3,
                               ncol = dim(Z[[i]])[2]), 2, mean))
    }
  }

  partial_g5_alpha <- list()
  part1_partial <- (1 - TRT) * A[, n_time_points]
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    if (dim_z > 1) {
      partial_vec <- part1_partial * (Expect_AZ_X[[i]] -
                                        Expect_A_X * Z1s_pred[[i]])
      partial_g5_alpha_1 <-  apply(partial_vec, 2, mean, na.rm = TRUE)
      partial_g5_alpha_x <- matrix(rep(NA, dim_z * dim(X)[2]), ncol = dim_z)
      for (j in 1:dim_z) {
        partial_g5_alpha_x[, j] <- apply(t(X) %*%
                      diag(as.vector(partial_vec[, j])), 1, mean, na.rm = TRUE)
      }
      partial_g5_alpha[[i]] <-  solve(sigma_mats_Z[[i]]) %*%
        t(rbind(partial_g5_alpha_1, partial_g5_alpha_x, partial_g5_alpha_1))
    } else {
      partial_vec <- part1_partial * (Expect_AZ_X[[i]] -
                                        Expect_A_X * Z1s_pred[[i]])
      partial_g5_alpha_1 <-  mean(partial_vec, na.rm = TRUE)
      partial_g5_alpha_x <- apply(t(X) %*% diag(as.vector(partial_vec)),
                                  1, mean, na.rm = TRUE)
      partial_g5_alpha[[i]] <-  solve(sigma_mats_Z[[i]]) %*%
        c(partial_g5_alpha_1, partial_g5_alpha_x, partial_g5_alpha_1)
    }

  }

  partial_g5_gamma <- list()
  for (i in 1:n_time_points) {
    if (i == 1) {
      constant <- (1 - TRT) * A[, n_time_points]
      vec1 <- Expect_AA_X[[i]]
      partial_g5_gamma[[i]] <-  c(mean(constant * vec1, na.rm = TRUE),
                                  apply(t(X) %*%
                                          diag(as.vector(constant * vec1)),
                                        1, mean, na.rm = TRUE))
    } else {
      constant <- (1 - TRT) * A[, n_time_points]
      vec1 <- Expect_AA_X[[i]]
      vec3 <- Expect_AA_Z_X[[i]]
      partial_g5_gamma[[i]] <-  c(mean(constant * vec1, na.rm = TRUE),
                                  apply(t(X) %*%
                                          diag(as.vector(constant * vec1)),
                                        1, mean, na.rm = TRUE),
                                  apply(matrix(constant * vec3,
                                  ncol = dim(Z[[i]])[2]), 2, mean,
                                  na.rm = TRUE))
    }
  }

  partial_g2_alpha <- list()
  for (i in 2:n_time_points) {
    partial_g2_alpha[[i]] <- -t(covariate_long[[i]]) %*% covariate_long[[i]] / n
  }

  partial_g3_gamma <- list()
  for (i in 1:n_time_points) {
    if (i == 1) {
      design_mat <- cbind(rep(1, n), X)
      partial_g3_gamma[[i]] <- -t(design_mat) %*%
        (design_mat * preds_A_XZ_clean[[i]] * (1 - preds_A_XZ_clean[[i]])) / n
    } else {
      design_mat <- cbind(rep(1, n), X, Z[[i]])
      design_mat_sub <- design_mat[A[, (i - 1)] != 0, ]
      A_pred_sub <- preds_A_XZ_clean[[i]][A[, (i - 1)] != 0] # nolint
      partial_g3_gamma[[i]] <- -t(design_mat_sub) %*% (design_mat_sub *
                                                            A_pred_sub *
                                                          (1 - A_pred_sub)) / n
    }
  }

  # Calculate plug-in variance estimator for S_{++} without residual term
  tau_est0 <- res0
  data_long <- cbind(data, subj = seq_len(nrow(data)))
  data_long <- reshape2::melt(data_long,
                   id.vars = c(X_col_names, "TRT", "Y", A_col_names, "subj"),
                   variable.name = "Z")
  temp <- list()
  temp[[1]] <- g2[[1]]
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    if (dim_z > 1) {
      res <- list()
      for (j in 1:dim_z) {
        res[[j]] <- matrix(0, nrow = n, ncol = ncol(g2[[i]][[j]]))
        res[[j]][!is.na(data_long$value[data_long$Z == Z_col_names[[i]][j]]),
                 ] <- g2[[i]][[j]]
      }
      temp[[i]] <- res
    } else {
      temp[[i]] <- matrix(0, nrow = n, ncol = ncol(g2[[i]]))
      temp[[i]][!is.na(data_long$value[data_long$Z == Z_col_names[[i]]]),
                ] <- g2[[i]]
    }

  }

  se0 <- 0
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    if (dim_z > 1) {
      for (j in 1:dim_z) {
        se0 <- se0 - temp[[i]][[j]] %*% solve(partial_g2_alpha[[i]]) %*%
          (partial_g4_alpha[[i]][j, ] -
             tau_est0 * partial_g5_alpha[[i]][j, ]) / mean(g5)
      }
    } else {
      se0 <- se0 - temp[[i]] %*% solve(partial_g2_alpha[[i]]) %*%
        t(partial_g4_alpha[[i]] - tau_est0 * partial_g5_alpha[[i]]) / mean(g5)
    }
  }

  for (i in 1:n_time_points) {
    se0 <- se0 - g3[[i]] %*% solve(partial_g3_gamma[[i]]) %*%
      (partial_g4_gamma[[i]] - tau_est0 * partial_g5_gamma[[i]]) / mean(g5)
  }
  se0 <- se0 + 1 / mean(g5) * (g4 - tau_est0 * g5)

  ##---------------------------------------------------------------------------
  ##   Variance calculation for First Term E(Y1|A^{(3)}==1)
  ##---------------------------------------------------------------------------

  # fit parametric model for Y
  # Predict Z using models.Z.X
  Z0s_pred <- list() # nolint
  for (i in 2:n_time_points) {
    Z0s_pred[[i]] <- predict(models_Z_X[[i]],
                             newdata = data.frame(X, TRT = rep(0, n)))
  }
  # Estimate alpha of Z_i given T for 1 (intercept) and Xs
  coefs_Z0_1X <- list()  # nolint
  for (i in 2:n_time_points) {
    coef_raw <- matrix(coef(models_Z_X[[i]]), ncol = dim(Z[[i]])[2])
    coefs_Z0_1X[[i]] <- matrix(coef_raw[1:(dim(X)[2] + 1), ],
                               ncol = dim(Z[[i]])[2])
  }
  # Calculate required integrations and assigning names for each column
  Expect_res <- apply(X, 1, Expect_function1D_BU, n_time_points = n_time_points,
                      gammas = coefs_A_XZ, alphas = coefs_Z0_1X,
                      Sigmas = sigma_mats_Z)
  Expect_res_t <- t(Expect_res)  # nolint
  names_vec <- NULL
  for (i in 2:n_time_points) {
    z_dim <- dim(Z[[i]])[2]
    names_prob <- paste("prob", i, sep = "")
    names_expa <- paste("expa", i, sep = "")
    names_other <- paste(c("expz", "expaz"), i, sep = "")
    names_expz <- c(paste(names_other[1], 1:z_dim, sep = ""))
    names_expaz <- c(paste(names_other[2], 1:z_dim, sep = ""))
    names_vec <- c(names_vec, names_prob, names_expz,
                   names_expa, names_expaz)
  }
  colnames(Expect_res_t) <- names_vec
  Expect_res_t <- cbind(Expect_res_t,prob1_expa1)
  names_vec <- c(names_vec, c("prob1","expa1"))



  # Calculate the probability of adherence at the end of study
  probs_vec <- paste("prob", 1:n_time_points, sep = "")
  Expect_probs <- Expect_res_t[, probs_vec]   # nolint
  Expect_A_X <- 1  # nolint
  for (i in 1:n_time_points) {
    Expect_A_X <- Expect_A_X * Expect_probs[, i]  # nolint
  }

  # Calculate several integrals
  Expect_AZ_X <- list()   # nolint
  Expect_AA_X <- list()   # nolint
  Expect_AA_Z_X <- list() # nolint
  for (i in 2:n_time_points) {
    names_target <- paste(c("expz", "expa", "expaz"), i, sep = "")
    prob_remove <- paste("prob", i, sep = "")
    Expect_AZ_X[[i]] <- Expect_A_X *
      Expect_res_t[, grepl(names_target[1], names_vec)] /
      Expect_res_t[, prob_remove]
    Expect_AA_Z_X[[i]] <-  Expect_A_X *
      Expect_res_t[, grepl(names_target[3], names_vec)] /
      Expect_res_t[, prob_remove]
  }

  for (i in 1:n_time_points) {
      names_target <- paste(c("expz", "expa", "expaz"), i, sep = "")
      prob_remove <- paste("prob", i, sep = "")
      Expect_AA_X[[i]] <- Expect_A_X *
      Expect_res_t[, grepl(names_target[2], names_vec)] /
      Expect_res_t[, prob_remove]
  }
  #-- End: calculate integration ----------------------------------------------
  # Calcuate estimator for the first term
  res1 <- sum(TRT * A[, n_time_points] * Expect_A_X * Y) /
    sum(TRT * A[, n_time_points] * Expect_A_X)

  # Prepare for plug-in variance estimator
  # g1,g2,g3, g4-res1*g5 are estimating equations

  #Same with the first term
  g4 <- TRT * A[, n_time_points] * Expect_A_X * Y
  g5 <- TRT * A[, n_time_points] * Expect_A_X


  # Estimating expection of deriatives using sample averages
  partial_g4_alpha <- list()
  part1_partial <- TRT * A[, n_time_points] * Y
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    partial_vec <- matrix(part1_partial * (Expect_AZ_X[[i]] - Expect_A_X *
                                             Z0s_pred[[i]]), ncol = dim_z)
    partial_g4_alpha_1 <-  apply(partial_vec, 2, mean, na.rm = TRUE)
    partial_g4_alpha_x <- matrix(rep(NA, dim_z * dim(X)[2]), ncol = dim_z)
    for (j in 1:dim_z) {
      partial_g4_alpha_x[, j] <- apply(t(X) %*%
                      diag(as.vector(partial_vec[, j])), 1, mean, na.rm = TRUE)
    }
    partial_g4_alpha[[i]] <- solve(sigma_mats_Z[[i]]) %*%
      t(rbind(partial_g4_alpha_1, partial_g4_alpha_x, partial_g4_alpha_1))
  }

  partial_g4_gamma <- list()
  for (i in 1:n_time_points) {
    if (i == 1) {
      constant <- TRT * A[, n_time_points] * Y
      vec1 <- Expect_AA_X[[i]]
      partial_g4_gamma[[i]] <- c(mean(constant * vec1, na.rm = TRUE),
                                 apply(t(X) %*%
                                         diag(as.vector(constant * vec1)),
                                       1, mean, na.rm = TRUE))
    } else {
      constant <- TRT * A[, n_time_points] * Y
      vec1 <- Expect_AA_X[[i]]
      vec3 <- Expect_AA_Z_X[[i]]
      partial_g4_gamma[[i]] <- c(mean(constant * vec1, na.rm = TRUE),
                                apply(t(X) %*% diag(as.vector(constant * vec1)),
                                1, mean, na.rm = TRUE),
                                apply(matrix(constant * vec3,
                                ncol = dim(Z[[i]])[2]), 2, mean, na.rm = TRUE))
    }
  }

  partial_g5_alpha <- list()
  part1_partial <- TRT * A[, n_time_points]
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    partial_vec <- matrix(part1_partial * (Expect_AZ_X[[i]] - Expect_A_X *
                                             Z0s_pred[[i]]), ncol = dim_z)
    partial_g5_alpha_1 <-  apply(partial_vec, 2, mean, na.rm = TRUE)
    partial_g5_alpha_x <- matrix(rep(NA, dim_z * dim(X)[2]), ncol = dim_z)
    for (j in 1:dim_z) {
      partial_g5_alpha_x[, j] <- apply(t(X) %*%
                      diag(as.vector(partial_vec[, j])), 1, mean, na.rm = TRUE)
    }
    partial_g5_alpha[[i]] <- solve(sigma_mats_Z[[i]]) %*%
      t(rbind(partial_g5_alpha_1, partial_g5_alpha_x, partial_g5_alpha_1))
  }

  partial_g5_gamma <- list()
  for (i in 1:n_time_points) {
    if (i == 1) {
      constant <- TRT * A[, n_time_points]
      vec1 <- Expect_AA_X[[i]]
      partial_g5_gamma[[i]] <-  c(mean(constant * vec1, na.rm = TRUE),
                                  apply(t(X) %*%
                                          diag(as.vector(constant * vec1)),
                                        1, mean, na.rm = TRUE))
    } else {
    constant <- TRT * A[, n_time_points]
    vec1 <- Expect_AA_X[[i]]
    vec3 <- Expect_AA_Z_X[[i]]
    partial_g5_gamma[[i]] <-  c(mean(constant * vec1, na.rm = TRUE),
                              apply(t(X) %*% diag(as.vector(constant * vec1)),
                                      1, mean, na.rm = TRUE),
                                apply(matrix(constant * vec3,
                                  ncol = dim(Z[[i]])[2]), 2,
                                  mean, na.rm = TRUE))
    }
  }

  tau_est1 <- res1

  se1 <- 0
  for (i in 2:n_time_points) {
    dim_z <- dim(Z[[i]])[2]
    if (dim_z > 1) {
      for (j in 1:dim_z) {
        se1 <- se1 - temp[[i]][[j]] %*% solve(partial_g2_alpha[[i]]) %*%
          (partial_g4_alpha[[i]][j, ] -
             tau_est1 * partial_g5_alpha[[i]][j, ]) / mean(g5)
      }
    } else {
      se1 <- se1 - temp[[i]] %*% solve(partial_g2_alpha[[i]]) %*%
        t(partial_g4_alpha[[i]] - tau_est1 * partial_g5_alpha[[i]]) / mean(g5)
    }

  }

  for (i in 1:n_time_points) {
    se1 <- se1 - g3[[i]] %*% solve(partial_g3_gamma[[i]]) %*%
      (partial_g4_gamma[[i]] - tau_est1 * partial_g5_gamma[[i]]) / mean(g5)
  }
  se1 <- se1 + 1 / mean(g5) * (g4 - tau_est1 * g5)

  res <- res1 - res0
  se  <- sd(se1 - se0, na.rm = TRUE) / sqrt(n)
  se1_res <- sd(se1, na.rm = TRUE) / sqrt(n)
  se0_res <- sd(se0, na.rm = TRUE) / sqrt(n)
  RVAL <- list(trt_diff = res,  # nolint
             se = se,
             res1 = res1,
             res0 = res0,
             se_res1 = se1_res,
             se_res0 = se0_res
  )
  return(RVAL)
}
