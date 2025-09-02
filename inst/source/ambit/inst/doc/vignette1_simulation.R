## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ambit)
library(ggplot2)

## -----------------------------------------------------------------------------
#Set the number of observations
n <-2000
#Set the width of the grid
Delta<-0.1

#Determine the trawl function
trawlfct="Exp"
trawlfct_par <-0.5

#Choose the marginal distribution
distr<-"Gauss"
#mean 0, std 1
distr_par<-c(0,1)

#Simulate the path
set.seed(233)
path <- sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par)$path

#Plot the path
df <- data.frame(time = seq(0,n,1), value=path)
p <- ggplot(df, aes(x=time, y=path))+
    geom_line()+
    xlab("l")+
    ylab("Trawl process")
p

#Plot the empirical acf and superimpose the theoretical one

#Plot the acf
my_acf <- acf(path, plot = FALSE)
my_acfdf <- with(my_acf, data.frame(lag, acf))
#Confidence limits
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(n)
q <- ggplot(data = my_acfdf, mapping = aes(x = lag, y = acf)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))+
        geom_hline(yintercept=conf.lims, lty=2, col='blue') +
        geom_function(fun = function(x) acf_Exp(x*Delta,trawlfct_par), colour="red", size=1.2)+
        xlab("Lag")+
        ylab("Autocorrelation")
q


## -----------------------------------------------------------------------------
#Set the number of observations
n <-2000
#Set the width of the grid
Delta<-0.1

#Determine the trawl function
trawlfct_par <-0.5
a <- function(x){exp(-trawlfct_par*x)}

#Choose the marginal distribution
distr<-"Gauss"
#mean 0, std 1
distr_par<-c(0,1)

#Simulate the path
set.seed(233)
path <- sim_weighted_trawl_gen(n, Delta, trawlfct_gen=a, distr, distr_par)$path

#Plot the path
df <- data.frame(time = seq(0,n,1), value=path)
p <- ggplot(df, aes(x=time, y=path))+
    geom_line()+
    xlab("l")+
    ylab("Trawl process")
p

#Plot the empirical acf and superimpose the theoretical one

#Plot the acf
my_acf <- acf(path, plot = FALSE)
my_acfdf <- with(my_acf, data.frame(lag, acf))
#Confidence limits
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(n)
q <- ggplot(data = my_acfdf, mapping = aes(x = lag, y = acf)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))+
        geom_hline(yintercept=conf.lims, lty=2, col='blue') +
        geom_function(fun = function(x) acf_Exp(x*Delta,trawlfct_par), colour="red", size=1.2)+
        xlab("Lag")+
        ylab("Autocorrelation")
q


