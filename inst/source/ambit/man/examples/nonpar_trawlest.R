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

#Estimate the trawl function
my_lag <- 100+1
PoiEx_trawl <- nonpar_trawlest(Poi_data, my_delta, lag=my_lag)$a_hat

#Plot the estimated trawl function and superimpose the true one
l_seq <- seq(from = 0,to = (my_lag-1), by = 1)
esttrawlfct.data <- base::data.frame(l=l_seq[1:31],
                               value=PoiEx_trawl[1:31])
p1 <- ggplot2::ggplot(esttrawlfct.data, ggplot2::aes(x=l,y=value))+
  ggplot2::geom_point(size=3)+
  ggplot2::geom_function(fun = function(x) acf_Exp(x*my_delta,my_lambda), colour="red", size=1.5)+
  ggplot2::xlab("l")+
  ggplot2::ylab(latex2exp::TeX("$\\hat{a}(\\cdot)$ for Poisson trawl process"))
p1
}
