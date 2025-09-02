##Simulate a trawl process
##Determine the sampling grid
my_n <- 1000
my_delta <- 0.1
my_t <- my_n*my_delta

###Choose the model parameter
#Exponential trawl function:
my_lambda <- 2
#Poisson marginal distribution trawl
my_v <- 1

#Set the seed
set.seed(123)
#Simulate the trawl process
Poi_data <- sim_weighted_trawl(my_n, my_delta,
                               "Exp", my_lambda, "Poi", my_v)$path

#Estimate the trawl function
my_lag <- 100+1
trawl <- nonpar_trawlest(Poi_data, my_delta, lag=my_lag)$a_hat

#Estimate the derivative of the trawl function
trawl_deriv <- trawl_deriv(Poi_data, my_delta, lag=100)
