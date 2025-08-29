#Ac3net: 
#This R package allows inferring directional conservative causal core network from large scale data.
#The inferred network consists of only direct physical interactions.
## Copyright (C) January 2011 Gokmen Altay <altayscience@gmail.com>
## This program is a free software for only academic useage but not for commercial useage; you can redistribute it and/or
## modify it under the terms of the GNU GENERAL PUBLIC LICENSE
## either version 3 of the License, or any later version.
##
## This program is distributed WITHOUT ANY WARRANTY; 
## You can get a copy of the GNU GENERAL PUBLIC LICENSE
## from
## http://www.gnu.org/licenses/gpl.html
## See the licence information for the dependent package from
## igraph package itself.

#takes an adjacency matrix and returns the absolute maximum correlated partner of each variable on the rows.
Ac3net.cutoff <- function(mim, ratio_ = 0.002, PCmincutoff=0.6, PCmaxcutoff=0.96)
{
  diag(mim) <-0
  ccc <- as.vector(mim) #might not be symmetric 
  ccc <- abs(ccc[ccc != 0]) #diagonal removed
  x<-sort(ccc, decreasing=T)
  a<- nrow(mim)
  a<- a*(a-1)/2
  rnratio <- round(ratio_*a)
  cutoff <- x[rnratio]
  if(cutoff < PCmincutoff) cutoff <- PCmincutoff
  if(cutoff > PCmaxcutoff) cutoff <- PCmaxcutoff
  return(cutoff)
}
