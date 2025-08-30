test_that("adace main function", {
  #### main function should return a list of 6 elements ####
  library(MASS)
  library(pracma)
  set.seed(123)
  j<- 200
  p_z <- 6 ## dimension of Z at each time point
  n_t <- 4 ## number of time points
  alphas <- list()
  gammas <- list()
  z_para <- c(-1/p_z, -1/p_z, -1/p_z, -1/p_z, -0.5/p_z,-0.5/p_z, -0.5/p_z,
              -0.5/p_z)
  Z <- list()
  beta = c(0.2, -0.3, -0.01, 0.02, 0.03, 0.04, rep(rep(0.02,p_z), n_t))
  beta_T = -0.2
  sd_z_x = 0.4
  X = mvrnorm(j, mu=c(1,5,6,7,8), Sigma=diag(1,5))
  TRT = rbinom(j, size = 1,  prob = 0.5)
  Y_constant <- beta[1]+(X%*%beta[2:6])
  Y0 <- 0
  Y1 <- 0
  A <- A1 <- A0 <- matrix(NA, nrow = j, ncol = n_t)
  gamma <- c(1,-.1,-0.05,0.05,0.05,.05)
  A0[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
                                                    (X %*% gamma[2:6])))))
  A1[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
                                                    (X %*% gamma[2:6])))))
  A[,1] <- A1[,1]*TRT + A0[,1]*(1-TRT)

  for(i in 2:n_t){
    alphas[[i]] <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04, -0.4),
                              p_z),ncol=p_z)
    gammas[[i]] <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(z_para[i],p_z))
    Z0 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,]) + mvrnorm(j, mu = rep(0,p_z),
                                                      Sigma = diag(sd_z_x,p_z))
    Z1 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,])+alphas[[i]][7,] +
      mvrnorm(j, mu = rep(0,p_z), Sigma = diag(sd_z_x,p_z))
    Z[[i]] <- Z1*TRT + Z0*(1-TRT)
    Y0 <- (Y0 + Z0 %*% matrix(beta[ (7 + (i-1)*p_z): (6+p_z*i)],ncol = 1) )[,1]
    Y1 <- (Y1 + Z1 %*% matrix(beta[ (7 + (i-1)*p_z): (6+p_z*i)],ncol = 1) )[,1]
    A0[,i] <- rbinom(j, size = 1,
                     prob = 1/(1+exp(-(gammas[[i]][1]+(X%*%gammas[[i]][2:6])+
                                         Z0%*%matrix(gammas[[i]][7: (7+p_z-1)],
                                                     ncol=1))[,1])))*A0[,i-1]
    A1[,i] <- rbinom(j, size = 1,
                     prob = 1/(1+exp(-(gammas[[i]][1]+(X%*%gammas[[i]][2:6])+
                                         Z1%*%matrix(gammas[[i]][7: (7+p_z-1)],
                                                     ncol=1))[,1])))*A1[,i-1]

    A[,i] <- A1[,i]*TRT + A0[,i]*(1-TRT)
  }
  Y0 <- Y0 + rnorm(j, mean = 0, sd = 0.3) + Y_constant
  Y1 <- Y1 + + beta_T + rnorm(j, mean = 0, sd = 0.3) + Y_constant

  Y <- as.vector( Y1*TRT+Y0*(1-TRT))

  for(i in 2:n_t){
    Z[[i]][A[,(i-1)]==0,] <- NA
  }

  Z[[1]] <- matrix(NA, nrow=nrow(Z1),ncol=ncol(Z1))

  Y[A[,n_t] == 0] <- NA
  # estimate the treatment difference
  fit1 <- est_S_Plus_Plus_MethodA(X, A, Z, Y, TRT)
  fit2 <- est_S_Star_Plus_MethodA(X, A, Z, Y, TRT)
  fit3 <- est_S_Plus_Plus_MethodB(X, A, Z, Y, TRT)
  fit4 <- est_S_Star_Plus_MethodB(X, A, Z, Y, TRT)
  # Test class
  expect_equal(class(fit1), "list")
  expect_equal(class(fit2), "list")
  expect_equal(class(fit3), "list")
  expect_equal(class(fit4), "list")
  expect_equal(sum(is.na(fit1)), 0)
  expect_equal(sum(is.na(fit2)), 0)
  expect_equal(sum(is.na(fit3)), 0)
  expect_equal(sum(is.na(fit4)), 0)

  # Test number of elements
  expect_equal(length(fit1), 6)
  expect_equal(length(fit2), 6)
  expect_equal(length(fit3), 6)
  expect_equal(length(fit4), 6)
  #### Test utility function ####
  A <- mat_vec(1:5)
  expect_equal(Rank(A), 5)
  expect_equal(NA_replace(rep(NA, 3)), c(999, 999, 999))
  expect_equal(sum(round(inv_svd(A) - solve(A), 5) == matrix(0, 5, 5)), 25)
  #### Test utitlity function for dim(Z)==1 ####
  al1 <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04), 1), ncol = 1)
  al2 <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04), 1), ncol = 1)
  al3 <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04), 1), ncol = 1)
  alps <- list(al1, al2, al3)
  be <- c(0.2, -0.3, -0.01, 0.02, 0.03, 0.04, rep(0.02, 1),
         rep(0.04, 1), rep(0.07, 1))
  gam1 <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(-1 / 3, 1))     #setting 1
  gam2 <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(-2 / 4, 1))     #setting 1
  gam3 <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(-2.5 / 5, 1))   #setting 1
  gams <- list(gam1, gam2, gam3)
  sd_z_x <- 0.4
  sigs <- list(diag(sd_z_x, 1), diag(sd_z_x, 1), diag(sd_z_x, 1))
  exp_res <- Expect_function1D_MA_1(X[1, , drop = FALSE], 3, gams, alps, sigs)
  expect_equal(class(exp_res), "numeric")
  expect_equal(sum(is.na(exp_res)), 0)

  #### Test main functions for dim(Z)==1 ####
  set.seed(12345)
  j<- 200
  p_z <- 1 ## dimension of Z at each time point
  n_t <- 3 ## number of time points
  alphas <- list()
  gammas <- list()
  z_para <- c(-1/p_z, -1/p_z, -1/p_z, -1/p_z, -0.5/p_z,-0.5/p_z, -0.5/p_z,
              -0.5/p_z)
  Z <- list()
  beta = c(0.2, -0.3, -0.01, 0.02, 0.03, 0.04, rep(rep(0.02,p_z), n_t))
  beta_T = -0.2
  sd_z_x = 0.4
  X = mvrnorm(j, mu=c(1,5,6,7,8), Sigma=diag(1,5))
  TRT = rbinom(j, size = 1,  prob = 0.5)
  Y_constant <- beta[1]+(X%*%beta[2:6])
  Y0 <- 0
  Y1 <- 0
  A <- A1 <- A0 <- matrix(NA, nrow = j, ncol = n_t)
  gamma <- c(1,-.1,-0.05,0.05,0.05,.05)
  A0[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
                                                    (X %*% gamma[2:6])))))
  A1[,1] <- rbinom(j, size = 1, prob = 1/(1+exp(-(gamma[1] +
                                                    (X %*% gamma[2:6])))))
  A[,1] <- A1[,1]*TRT + A0[,1]*(1-TRT)

  for(i in 2:n_t){
    alphas[[i]] <- matrix(rep(c(2.3, -0.3, -0.01, 0.02, 0.03, 0.04, -0.4),p_z),
                          ncol=p_z)
    gammas[[i]] <- c(1, -0.1, 0.2, 0.2, 0.2, 0.2, rep(z_para[i],p_z))
    Z0 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,]) + mvrnorm(j, mu = rep(0,p_z),
                                                      Sigma = diag(sd_z_x,p_z))
    Z1 <- alphas[[i]][1,]+(X%*%alphas[[i]][2:6,])+alphas[[i]][7,] +
      mvrnorm(j, mu = rep(0,p_z), Sigma = diag(sd_z_x,p_z))
    Z[[i]] <- Z1*TRT + Z0*(1-TRT)
    Y0 <- (Y0 + Z0 %*% matrix(beta[ (7 + (i-1)*p_z): (6+p_z*i)],ncol = 1) )[,1]
    Y1 <- (Y1 + Z1 %*% matrix(beta[ (7 + (i-1)*p_z): (6+p_z*i)],ncol = 1) )[,1]
    A0[,i] <- rbinom(j, size = 1,
                     prob = 1/(1+exp(-(gammas[[i]][1]+(X%*%gammas[[i]][2:6])+
                                         Z0%*%matrix(gammas[[i]][7: (7+p_z-1)],
                                                     ncol=1))[,1])))*A0[,i-1]
    A1[,i] <- rbinom(j, size = 1,
                     prob = 1/(1+exp(-(gammas[[i]][1]+(X%*%gammas[[i]][2:6])+
                                         Z1%*%matrix(gammas[[i]][7: (7+p_z-1)],
                                                     ncol=1))[,1])))*A1[,i-1]

    A[,i] <- A1[,i]*TRT + A0[,i]*(1-TRT)
  }
  Y0 <- Y0 + rnorm(j, mean = 0, sd = 0.3) + Y_constant
  Y1 <- Y1 + + beta_T + rnorm(j, mean = 0, sd = 0.3) + Y_constant

  Y <- as.vector( Y1*TRT+Y0*(1-TRT))

  for(i in 2:n_t){
    Z[[i]][A[,(i-1)]==0,] <- NA
  }

  Z[[1]] <- matrix(NA, nrow=nrow(Z1),ncol=ncol(Z1))

  Y[A[,n_t] == 0] <- NA
  # estimate the treatment difference
  fit1 <- est_S_Plus_Plus_MethodA(X, A, Z, Y, TRT)
  fit2 <- est_S_Star_Plus_MethodA(X, A, Z, Y, TRT)
  fit3 <- est_S_Plus_Plus_MethodB(X, A, Z, Y, TRT)
  fit4 <- est_S_Star_Plus_MethodB(X, A, Z, Y, TRT)
  # Test class
  expect_equal(class(fit1), "list")
  expect_equal(class(fit2), "list")
  expect_equal(class(fit3), "list")
  expect_equal(class(fit4), "list")
  expect_equal(sum(is.na(fit1)), 0)
  expect_equal(sum(is.na(fit2)), 0)
  expect_equal(sum(is.na(fit3)), 0)
  expect_equal(sum(is.na(fit4)), 0)

  # Test number of elements
  expect_equal(length(fit1), 6)
  expect_equal(length(fit2), 6)
  expect_equal(length(fit3), 6)
  expect_equal(length(fit4), 6)

})
