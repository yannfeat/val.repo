\donttest{
#Simulation of a Gaussian trawl process with exponential trawl function
n<-2000
Delta<-0.1
trawlfct="Exp"
trawlfct_par <-0.5
distr<-"Gauss"
distr_par<-c(0,1) #mean 0, std 1
set.seed(233)
path <- sim_weighted_trawl(n, Delta, trawlfct, trawlfct_par, distr, distr_par)$path
#Plot the path
library(ggplot2)
df <- data.frame(time = seq(0,n,1), value=path)
p <- ggplot(df, aes(x=time, y=path))+
  geom_line()+
  xlab("l")+
  ylab("Trawl process")
  p
}
