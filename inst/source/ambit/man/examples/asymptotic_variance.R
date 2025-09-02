#Compute the asymptotic variance at time t for an exponential trawl with
#parameter 2; here we assume that the fourth cumulant equals 1.
av<-asymptotic_variance(t=1, c4=1, varlevyseed=1, trawlfct="Exp", trawlfct_par=2)
#Print the av
av$v
#Print the four components of the asymptotic variance separately
av$v1
av$v2
av$v3
av$v4

#Note that v=v1+v2+v3+v4
av$v
av$v1+av$v2+av$v3+av$v4
