## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ambit)
library(ggplot2)
library(latex2exp)


## -----------------------------------------------------------------------------
###Choosing the sampling scheme
my_n <- 5000
my_delta <- 0.1
my_t <- my_n*my_delta

###Choosing the model parameter
#Exponential trawl:
my_lambda <- 1
###Poisson-Exponential trawl
my_v <- 1


##Gaussian-Exponential trawl
my_mu <- 0
my_sigma <-1

#Negative binomial model

my_theta <- 0.2
my_m <- (1-my_theta)^2/my_theta


#Set the seed
set.seed(123)

Poi_data<-ambit::sim_weighted_trawl(my_n, my_delta, "Exp", my_lambda, "Poi", my_v)$path
NB_data<-ambit::sim_weighted_trawl(my_n, my_delta, "Exp", my_lambda, "NegBin", c(my_m,my_theta))$path
Gau_data<-ambit::sim_weighted_trawl(my_n, my_delta, "Exp", my_lambda, "Gauss", c(my_mu,my_sigma))$path


## -----------------------------------------------------------------------------
#Plot the path
df1 <- base::data.frame(time = base::seq(0,my_n,1), value=Poi_data)
p1 <- ggplot(df1, aes(x=time, y=Poi_data))+
    geom_line()+
    xlab("l")+
    ylab("Poisson trawl process")
p1

## -----------------------------------------------------------------------------
df2 <- base::data.frame(time = base::seq(0,my_n,1), value=NB_data)
p2 <- ggplot(df2, aes(x=time, y=NB_data))+
    geom_line()+
    xlab("l")+
    ylab("Negative binomial trawl process")
p2

## -----------------------------------------------------------------------------
df3 <- base::data.frame(time = base::seq(0,my_n,1), value=Gau_data)
p3 <- ggplot(df3, aes(x=time, y=Gau_data))+
    geom_line()+
    xlab("l")+
    ylab("Gaussian trawl process")
p3




## -----------------------------------------------------------------------------
my_lag <- 100+1

PoiEx_trawl <- nonpar_trawlest(Poi_data, my_delta, lag=my_lag)$a_hat

l_seq <- seq(from = 0,to = (my_lag-1), by = 1)
esttrawlfct.data <- data.frame(l=l_seq[1:31],
                        value=PoiEx_trawl[1:31])
p1 <- ggplot(esttrawlfct.data, aes(x=l,y=value))+
  geom_point(size=3)+
  geom_function(fun = function(x) acf_Exp(x*my_delta,my_lambda), colour="red", size=1.5)+
  xlab("l")+
  ylab(TeX("$\\hat{a}(\\cdot)$ for Poisson trawl process"))
p1

## -----------------------------------------------------------------------------
my_lag <- 100+1

NBEx_trawl <- nonpar_trawlest(NB_data, my_delta, lag=my_lag)$a_hat

l_seq <- seq(from = 0,to = (my_lag-1), by = 1)
esttrawlfct.data <- data.frame(l=l_seq[1:31],
                        value=NBEx_trawl[1:31])
p2 <- ggplot(esttrawlfct.data, aes(x=l,y=value))+
  geom_point(size=3)+
  geom_function(fun = function(x) acf_Exp(x*my_delta,my_lambda), colour="red", size=1.5)+
  xlab("l")+
  ylab(TeX("$\\hat{a}(\\cdot)$ for NegBin trawl process"))
p2

## -----------------------------------------------------------------------------
my_lag <- 100+1

GaussEx_trawl <- nonpar_trawlest(Gau_data, my_delta, lag=my_lag)$a_hat

l_seq <- seq(from = 0,to = (my_lag-1), by = 1)
esttrawlfct.data <- data.frame(l=l_seq[1:31],
                        value=GaussEx_trawl[1:31])
p3 <- ggplot(esttrawlfct.data, aes(x=l,y=value))+
  geom_point(size=3)+
  geom_function(fun = function(x) acf_Exp(x*my_delta,my_lambda), colour="red", size=1.5)+
  xlab("l")+
  ylab(TeX("$\\hat{a}(\\cdot)$ for Gaussian trawl process"))
p3

## -----------------------------------------------------------------------------

#Checking length of return vector
my_lag <- 100+1

NBEx_trawl <- nonpar_trawlest(NB_data, my_delta, lag=my_lag)$a_hat

c4_est <- c4est(NB_data, my_delta)

print("The fourth cumulant is estimated to be:")
c4_est

print("The asymptotic variance for t=1 is estimated to be:")

asymptotic_variance_est(t=1, c4=c4_est, varlevyseed=1, Delta=my_delta, avector=NBEx_trawl)$v

print("The feasible test statistic for t=0 is estimated to be:")
test_asymnorm_est(NB_data, my_delta, trawlfct="Exp", trawlfct_par=0.5)[1]

