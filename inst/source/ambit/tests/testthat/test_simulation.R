#Test whether R and C++ addition in simulation leads the same result
set.seed(1)
n<-100
Delta<-0.1

#Case 1:
trawlfct="Exp"
trawlfct_par <-0.5
distr<-"Poi"
distr_par<-2


p<-function(x)(5*sin(x/0.1))

x<-sim_weighted_trawl_dev(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)
path1 <-x$path
path2<-x$path_Rcpp

for(i in 1:n){
  expect_equal(path1[i], path2[i],tolerance=0.0001)
}

####Checking that the development version and the final version coincide
set.seed(1)
path1 <-sim_weighted_trawl_dev(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path_Rcpp
set.seed(1)
path2 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
for(i in 1:n){
  expect_equal(path1[i], path2[i],tolerance=0.0001)
}

#Case 2:
trawlfct="Exp"
trawlfct_par <-0.5
distr<-"Gauss"
distr_par<-c(3,5)


p<-function(x)(5*exp(x))

x<-sim_weighted_trawl_dev(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)
path1 <-x$path
path2<-x$path_Rcpp

for(i in 1:n){
  expect_equal(path1[i], path2[i],tolerance=0.0001)
}

####Checking that the development version and the final version coincide
set.seed(1)
path1 <-sim_weighted_trawl_dev(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path_Rcpp
set.seed(1)
path2 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
for(i in 1:n){
  expect_equal(path1[i], path2[i],tolerance=0.0001)
}


###Checking first and second moments of simulated trawls
n<-2000
Delta<-0.1

trawlfct="Exp"
trawlfct_par <-0.5


p<-function(x)(1)

#Check Gamma
distr<-"Gamma"
shape <- 2
scale <- 3
distr_par<-c(2, 3)

set.seed(1135)
pathg <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(pathg), shape*scale*1/0.5, tolerance=0.1*3)
expect_equal(sqrt(stats::var(pathg)), sqrt(shape*scale^2)*1/sqrt(0.5), tolerance = 0.1)

shape <- 0.2
scale <- 3
distr_par<-c(0.2, 3)

set.seed(1133)
pathg <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(pathg), shape*scale*1/0.5, tolerance=0.1*3)
expect_equal(sqrt(stats::var(pathg)), sqrt(shape*scale^2)*1/sqrt(0.5), tolerance = 0.1)



#Check Gaussian
distr<-"Gauss"
#mean 0, std 1
distr_par<-c(0,1)
set.seed(102735)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), 0, tolerance=0.1*3)
expect_equal(sqrt(stats::var(path3)), 1/sqrt(0.5), tolerance = 0.1)

#mean 3, std 5
distr_par<-c(3,5)
set.seed(102135)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), 3*1/0.5,tolerance=0.1*5)
expect_equal(sqrt(stats::var(path3)), 5*1/sqrt(0.5), tolerance = 0.1*5)


#Check Cauchy
distr<-"Cauchy"
#location 0, scale 1
distr_par<-c(0,1)
set.seed(2735)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(stats::median(path3), 0, tolerance=0.1*3)

#location 3, scale 5
distr_par<-c(3,5)
set.seed(102)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(stats::median(path3), 3*1/0.5,tolerance=0.1*5)

#Check NIG
distr<-"NIG"
alpha <- 2
beta <- 1
delta <- 1
mu <- 0
gamma <- sqrt(alpha^2-beta^2)
distr_par<-c(alpha, beta, delta, mu)
set.seed(2735)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), (delta*beta/gamma+mu)/trawlfct_par, tolerance=0.1*3)

alpha <- 2
beta <- 1
delta <- 1
mu <- 10
gamma <- sqrt(alpha^2-beta^2)
distr_par<-c(alpha, beta, delta, mu)
set.seed(2735)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), (delta*beta/gamma+mu)/trawlfct_par, tolerance=0.1*3)

#Check Poi
distr<-"Poi"
#lambda 1
distr_par<-1
set.seed(1135)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), 1/0.5, tolerance=0.1*3)
expect_equal(sqrt(stats::var(path3)), 1/sqrt(0.5), tolerance = 0.1)

#lambda 4
distr_par<-4
set.seed(135)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), 4*1/0.5,tolerance=0.1*5)
expect_equal(sqrt(stats::var(path3)), sqrt(4)*1/sqrt(0.5), tolerance = 0.1*5)

#Check NegBin
distr<-"NegBin"
#m = 2, theta=0.3
distr_par<-c(2, 0.3)
m<-2; theta<-0.3
set.seed(11335)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), m*theta/(1-theta)*1/0.5, tolerance=0.1*3)
expect_equal(sqrt(stats::var(path3)), sqrt(m*theta/(1-theta)^2)*1/sqrt(0.5), tolerance = 0.4)

#m = 4, theta=0.1
distr_par<-c(4, 0.1)
m<-4; theta<-0.1
set.seed(13135)
path3 <-sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par, p)$path
expect_equal(base::mean(path3), m*theta/(1-theta)*1/0.5, tolerance=0.1*3)
expect_equal(sqrt(stats::var(path3)), sqrt(m*theta/(1-theta)^2)*1/sqrt(0.5), tolerance = 0.4)


####Testing the generic trawl simulation
n<-2000
Delta<-0.1

trawlfct_par <-0.5
distr<-"Gauss"
distr_par<-c(0,1) #mean 0, std 1
set.seed(233)

my_trawl <- function(x){exp(-trawlfct_par*x)}
path <- sim_weighted_trawl_gen(n, Delta, my_trawl,
                               distr, distr_par)$path

n<-2000
Delta<-0.1
trawlfct ="Exp"
trawlfct_par <-0.5
distr<-"Gauss"
distr_par<-c(0,1) #mean 0, std 1
set.seed(233)


path2 <- sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par,
                               distr, distr_par)$path

for(i in 1:n){
  expect_equal(path[i], path2[i], tolerance=0.1)
}

