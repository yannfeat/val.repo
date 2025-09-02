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

#Compute the test statistic for time t=0
##Either one can use:
test_asymnorm_est(Poi_data, my_delta,
                  trawlfct="Exp", trawlfct_par=my_lambda)[1]
#or:
test_asymnorm_est(Poi_data, my_delta,
                  trawlfct="Exp", trawlfct_par=my_lambda, k=0)
