\donttest{
##Simulate a trawl process
##Determine the sampling grid
my_n <- 5000
my_delta <- 0.1
my_t <- my_n*my_delta

###Choose the model parameter
#Exponential trawl function:
my_lambda <- 2
#Poisson marginal distribution trawl
my_v <- 1

#Set the seed
set.seed(1726)
#Simulate the trawl process
Poi_data<-ambit::sim_weighted_trawl(my_n, my_delta, "Exp", my_lambda, "Poi", my_v)$path

#Estimate the trawl set without bias correction
LebA1 <-LebA_est(Poi_data, my_delta)
LebA1

#Estimate the trawl set with bias correction
LebA2 <-LebA_est(Poi_data, my_delta, biascor=TRUE)
LebA2

#Note that Leb(A)=1/my_lambda for an exponential trawl
}
