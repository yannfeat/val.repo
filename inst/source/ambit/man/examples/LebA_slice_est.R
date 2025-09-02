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

#Estimate the trawl set and its two slices at time h=2 without bias correction
est1 <- LebA_slice_est(Poi_data, my_delta, h=2)
est1$LebA
est1$LebAintersection
est1$LebAsetdifference

#Estimate the trawl set and its two slices at time h=2 without bias correction
est2 <- LebA_slice_est(Poi_data, my_delta, h=2, biascor=TRUE)
est2$LebA
est2$LebAintersection
est2$LebAsetdifference

#Note that Leb(A)=1/my_lambda for an exponential trawl
}
