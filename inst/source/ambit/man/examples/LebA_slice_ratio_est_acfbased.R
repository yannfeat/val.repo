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

#Estimate the trawl set and its two slices at time h=0.5
est <- LebA_slice_ratio_est_acfbased(Poi_data, my_delta, h=0.5)
#Print the ratio LebAintersection/LebA
est$LebAintersection_ratio
#Print the ratio LebAsetdifference/LebA
est$LebAsetdifference_ratio
}
