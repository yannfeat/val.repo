#Test the non-par trawl estimation

#Create test data
set.seed(16254)

n <- 1000
beta <- 0.4  # Note that we require that beta \in (1/3,1)
Delta <- n^(-beta)


trawlfct="Exp"
trawlfct_par <-0.5
distr<-"Poi"
distr_par<-2

#We simulate data at time points 0, Delta, ...(n-1) Delta
data <- sim_weighted_trawl((n-1), Delta, trawlfct, trawlfct_par, distr, distr_par)$path

#Checking length of return vector
esttrawl <- ambit::nonpar_trawlest(data, Delta, lag =50)
expect_equal(length(esttrawl$a_hat),50,tolerance=0.01)

esttrawl_long1 <- ambit::nonpar_trawlest(data, Delta, lag =n-1)
expect_equal(length(esttrawl_long1$a_hat),n-1,tolerance=0.01)

esttrawl_long2 <- ambit::nonpar_trawlest(data, Delta, lag =n)
expect_equal(length(esttrawl_long2$a_hat),n-1,tolerance=0.01)

#Checking computations
my_cov <-function(x, lag){
  n <- length(x)
  samplemean <-sum(x)/n
  cov <-0
  for(k in 1:(n-lag)){
    cov <- cov + (data[lag+k]-samplemean)*(data[k]-samplemean)/n
  }
  return(cov)
}

acf_cov <- stats::acf(data, lag=50+1,type = c("covariance"), plot = FALSE)$acf[1]
expect_equal(my_cov(data, 0), acf_cov, tolerance=0.0001)
acf_cov <- stats::acf(data, lag=50+1,type = c("covariance"), plot = FALSE)$acf[5]
expect_equal(my_cov(data, 4), acf_cov, tolerance=0.0001)
acf_cov <- stats::acf(data, lag=50+1,type = c("covariance"), plot = FALSE)$acf[10]
expect_equal(my_cov(data, 9), acf_cov, tolerance=0.0001)

a0_alt <- -(my_cov(data,1)-my_cov(data,0))/Delta
expect_equal(a0_alt, esttrawl$a0_alt, tolerance=0.0001)

a5 <- -(my_cov(data,6)-my_cov(data,5))/Delta
expect_equal(a5, esttrawl$a_hat[6], tolerance=0.0001)

a10 <- -(my_cov(data,11)-my_cov(data,10))/Delta
expect_equal(a10, esttrawl$a_hat[11], tolerance=0.0001)

(stats::acf(data, lag=50+1,type = c("correlation"), plot = FALSE)$acf)[2:52]

#Test the RV computations for t=0
incr <-numeric(n)
for(i in 1:(n-1)){
  incr[i] <- data[i+1]-data[i]
}
rv <- sum(incr^2)/(2*n*Delta)
expect_equal(rv, esttrawl$a_hat[1], tolerance=0.001)


####################################
#Test the LebA estimation
data1 <- sim_weighted_trawl((n-1), Delta, trawlfct, trawlfct_par, distr, distr_par)$path
data2 <- sim_weighted_trawl((n-1), Delta, trawlfct, 2*trawlfct_par, distr, distr_par)$path
#expect_equal(LebA_est(data1,Delta), 1/trawlfct_par, tolerance=0.1)
#expect_equal(LebA_est(data2,Delta), 1/(2*trawlfct_par), tolerance=0.1)
expect_equal(LebA_est(data1,Delta), var(data1), tolerance=0.5)
expect_equal(LebA_est(data2,Delta), var(data2), tolerance=0.5)

expect_equal(LebA_est(data1,Delta), LebA_slice_est(data1, Delta,1)$LebA, tolerance=0.0001)
expect_equal(LebA_est(data2,Delta), LebA_slice_est(data2, Delta,1)$LebA, tolerance=0.0001)

x1<-LebA_slice_est(data1, Delta,1)$LebAintersection
x2<-LebA_slice_est(data1, Delta,1)$LebAsetdifference
expect_equal(LebA_slice_est(data1, Delta,1)$LebA, (x1+x2), tolerance=0.0001)

x1<-LebA_slice_est(data1, Delta,5)$LebAintersection
x2<-LebA_slice_est(data1, Delta,5)$LebAsetdifference
expect_equal(LebA_slice_est(data1, Delta,5)$LebA, (x1+x2), tolerance=0.0001)


data3 <- sim_weighted_trawl((n-1), Delta, trawlfct="LM", trawlfct_par=c(0.5, 3), distr, distr_par)$path
data4 <- sim_weighted_trawl((n-1), Delta, trawlfct="LM", trawlfct_par=c(2, 7), distr, distr_par)$path
#expect_equal(LebA_est(data3,Delta*0.01),0.5/(3-1), tolerance=0.1)
#expect_equal(LebA_est(data4,Delta), 2/(7-1), tolerance=0.1)
expect_equal(LebA_est(data3,Delta),var(data3), tolerance=0.4)
expect_equal(LebA_est(data4,Delta), var(data4), tolerance=0.4)



#Test the asymptotic variance computations from the CLT
a1<-asymptotic_variance(1, c4=1, varlevyseed=1, "Exp", 0.5)$v
a2<-asymptotic_variance(1, c4=1, varlevyseed=1, "Exp", 0.5)$v1+
  asymptotic_variance(1, c4=1, varlevyseed=1, "Exp", 0.5)$v2+
  asymptotic_variance(1, c4=1, varlevyseed=1, "Exp", 0.5)$v3+
  asymptotic_variance(1, c4=1, varlevyseed=1, "Exp", 0.5)$v4
expect_equal(a1,a2,tolerance=0.001)


a1<-asymptotic_variance(1, c4=1, varlevyseed=1, "supIG", c(0.5, 0.3))$v
a2<-asymptotic_variance(1, c4=1, varlevyseed=1, "supIG", c(0.5, 0.3))$v1+
  asymptotic_variance(1, c4=1, varlevyseed=1, "supIG", c(0.5, 0.3))$v2+
  asymptotic_variance(1, c4=1, varlevyseed=1, "supIG", c(0.5, 0.3))$v3+
  asymptotic_variance(1, c4=1, varlevyseed=1, "supIG", c(0.5, 0.3))$v4
expect_equal(a1,a2,tolerance=0.001)


a1<-asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 2))$v
a2<-asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 2))$v1+
  asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 2))$v2+
  asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 2))$v3+
  asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 2))$v4
expect_equal(a1,a2,tolerance=0.001)

#Test the numerical approximation of the asymptotic variance
asymptotic_variance_Exp <- function(t, c4, varlevyseed=1, lambda){
  v1 <- c4*exp(-lambda*t)
  v2 <- 2*varlevyseed*(2*lambda)^(-1)
  v3 <- 2*varlevyseed*t*exp(-2*lambda*t)
  v4 <- -2*varlevyseed*(2*lambda)^(-1)*exp(-2*lambda*t)
  v <- v1+v2+v3+v4
  return(list("v"=v, "v1"=v1, "v2"=v2, "v3"=v3, "v4"=v4))
}

a1<-asymptotic_variance_Exp(1, c4=1, varlevyseed=1, lambda=0.5)$v
a2<-asymptotic_variance_Exp(1, c4=1, varlevyseed=1, lambda=0.5)$v1+
  asymptotic_variance_Exp(1, c4=1, varlevyseed=1, lambda=0.5)$v2+
  asymptotic_variance_Exp(1, c4=1, varlevyseed=1, lambda=0.5)$v3+
  asymptotic_variance_Exp(1, c4=1, varlevyseed=1, lambda=0.5)$v4
expect_equal(a1,a2,tolerance=0.001)

a1<-asymptotic_variance_Exp(2, c4=5, varlevyseed=1.3, lambda=1)$v
a2<-asymptotic_variance_Exp(2, c4=5, varlevyseed=1.3, lambda=1)$v1+
  asymptotic_variance_Exp(2, c4=5, varlevyseed=1.3, lambda=1)$v2+
  asymptotic_variance_Exp(2, c4=5, varlevyseed=1.3, lambda=1)$v3+
  asymptotic_variance_Exp(2, c4=5, varlevyseed=1.3, lambda=1)$v4
expect_equal(a1,a2,tolerance=0.001)

asymptotic_variance_LM <- function(t, c4, varlevyseed=1, alpha, H){
  t=1; c4=1; varlevyseed=1; alpha=0.5; H=1.5
  v1 <- c4*trawl_LM(-t,alpha,H)
  v2 <- 2*varlevyseed*alpha*(2*H-1)^(-1)

  fct3 <- function(x){trawl_LM(-(t-x),alpha, H)*trawl_LM(-(t+x),alpha, H)}
  num3 <- integrate(fct3, 0, t)$value
  v3 <- 2*varlevyseed*num3

  fct4 <- function(x){trawl_LM(-(x-t),alpha, H)*trawl_LM(-(t+x),alpha, H)}
  num4 <- integrate(fct4, t, Inf)$value

  v4 <- -2*varlevyseed*num4
  v <- v1+v2+v3+v4
  return(list("v"=v, "v1"=v1, "v2"=v2, "v3"=v3, "v4"=v4))
}

a1<-asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v
a2<-asymptotic_variance(1, c4=1, varlevyseed=1, "LM", c(0.5, 1.5))$v
expect_equal(a1,a2,tolerance=0.001)

a1<-asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v
a2<-asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v1+
  asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v2+
  asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v3+
  asymptotic_variance_LM(1, c4=1, varlevyseed=1, alpha=0.5, H=1.5)$v4
expect_equal(a1,a2,tolerance=0.001)

a1<-asymptotic_variance_LM(2, c4=5, varlevyseed=1.3, alpha=1.3, H=2.5)$v
a2<-asymptotic_variance_LM(2, c4=5, varlevyseed=1.3, alpha=1.3, H=2.5)$v1+
  asymptotic_variance_LM(2, c4=5, varlevyseed=1.3, alpha=1.3, H=2.5)$v2+
  asymptotic_variance_LM(2, c4=5, varlevyseed=1.3, alpha=1.3, H=2.5)$v3+
  asymptotic_variance_LM(2, c4=5, varlevyseed=1.3, alpha=1.3, H=2.5)$v4
expect_equal(a1,a2,tolerance=0.001)


###########Testing the realised quarticity
rq <- rq(data, Delta)
expect_equal(length(rq),1,tolerance=0.0001)

c4_est <- c4est(data, Delta)
expect_equal(length(c4_est),1,tolerance=0.0001)

expect_equal(c4_est,distr_par,tolerance=0.7)

set.seed(2)
data2 <- sim_weighted_trawl((n-1), Delta, trawlfct, trawlfct_par, distr, distr_par*2)$path
c4_est2 <- c4est(data2, Delta)

expect_equal(c4_est,distr_par,tolerance=0.7*2)


###Testing the estimated asymptotic variance from the CLT
v<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat)$v
v1<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat)$v1
v2<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat)$v2
v3<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat)$v3
v4<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat)$v4

expect_equal(v, v1+v2+v3+v4, tolerance=0.00001)

##Test whether if N is supplied things work correctly, note that here lag=n=50

vN1<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=51)$v
expect_equal(v, vN1, tolerance=0.00001)

vN2<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=50)$v
expect_equal(v, vN2, tolerance=0.00001)

vN3<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=49)$v
expect_equal(v, vN3, tolerance=0.00001)

vN4<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=48)$v
expect_false(isTRUE(all.equal(v, vN4)))

vN5<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=40)$v
expect_false(isTRUE(all.equal(v, vN5)))

vN6<-asymptotic_variance_est(t=1, c4=1, varlevyseed=1, Delta=Delta, avector=esttrawl$a_hat, N=20)$v
expect_false(isTRUE(all.equal(v, vN6)))


#Test the feasible test statistic
test <-test_asymnorm_est_dev(ahat=esttrawl$a_hat[1], n, Delta, k=0, c4=c4est(data, Delta), varlevyseed=1, trawlfct="Exp", trawlfct_par=0.5, avector=esttrawl$a_hat)$x

test_alt <-sqrt(n*Delta/rq(data, Delta))*(esttrawl$a_hat[1]-1)

expect_equal(test, test_alt, tolerance=0.001)


#Test derivative estimation of modified version
my_lag <- 50
deriv1 <-trawl_deriv_mod(data, Delta=Delta, lag=my_lag)

deriv2 <- vector(mode = "numeric", length = my_lag)

for(i in 0:(my_lag-1)){
  tmp<-0
  for(k in (i+1):(n-2)){
    tmp <- tmp+ (data[k+1+1]-data[k+1])*(data[k-i+1]-data[k-i-1+1])
  }
  deriv2[i+1] <- tmp/(n*Delta^2)
}


for(i in 1:my_lag){
  expect_equal(deriv1[i], deriv2[i], tolerance=0.001)
}

#Test derivative estimation of standard version
my_lag <- 50
deriv3 <-trawl_deriv(data, Delta=Delta, lag=my_lag)
deriv4 <- vector(mode = "numeric", length = my_lag)

ahat <- nonpar_trawlest(data, Delta, my_lag+1)$a_hat
for(i in 1:my_lag){
  deriv4[i] <- (ahat[i+1]-ahat[i])/Delta
}

for(i in 1:my_lag){
  expect_equal(deriv3[i], deriv4[i], tolerance=0.001)
}


for(i in 1:my_lag){
  expect_equal(deriv1[i], deriv3[i], tolerance=0.7)
}


#####Testing the test statistic
test1 <- test_asymnorm_est(data, Delta, trawlfct, trawlfct_par)
expect_equal(length(test1), n-1, tolerance=0.001)

test1_biascor <- test_asymnorm_est(data, Delta, trawlfct, trawlfct_par, biascor =TRUE)
expect_equal(length(test1_biascor), n-2, tolerance=0.001)

test1b_biascor <- test_asymnorm_est(data, Delta, trawlfct, trawlfct_par, biascor =TRUE,k=0)
expect_equal(length(test1b_biascor), 1, tolerance=0.001)

test1c_biascor <- test_asymnorm_est(data, Delta, trawlfct, trawlfct_par, biascor =TRUE,k=10)
expect_equal(length(test1c_biascor), 1, tolerance=0.001)

##Comparing the test statistic with the development version
test_dev <-test_asymnorm_est_dev(ahat=esttrawl$a_hat[1], n, Delta, k=0, c4=c4est(data, Delta), varlevyseed=1, trawlfct="Exp", trawlfct_par=0.5, avector=esttrawl$a_hat)$x

test_alt <-test_asymnorm_est(data, Delta, trawlfct="Exp", trawlfct_par=0.5)[1]

expect_equal(test_dev, test_alt, tolerance=0.001)

#Specifying the time point directly
test_alt2 <-test_asymnorm_est(data, Delta, trawlfct="Exp", trawlfct_par=0.5, k=0)
expect_equal(test_dev, test_alt2, tolerance=0.001)

test_dev <- numeric(101)
test_alt <- numeric(101)
test_alt2 <- numeric(101)
esttrawl_long <- ambit::nonpar_trawlest(data, Delta, lag =n)
for(i in 0:101){
  test_dev[i+1] <-test_asymnorm_est_dev(ahat=esttrawl_long$a_hat[1+i], n, Delta, k=i, c4=c4est(data, Delta), varlevyseed=1, trawlfct="Exp", trawlfct_par=0.5, avector=esttrawl_long$a_hat)$x

  test_alt[i+1] <-test_asymnorm_est(data, Delta, trawlfct="Exp", trawlfct_par=0.5)[1+i]

  test_alt2[i+1] <-test_asymnorm_est(data, Delta, trawlfct="Exp", trawlfct_par=0.5, k=i)

  expect_equal(test_dev[i+1], test_alt[i+1], tolerance=0.1)
  expect_equal(test_alt2[i+1], test_alt[i+1], tolerance=0.1)

}

