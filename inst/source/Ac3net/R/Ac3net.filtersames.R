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

#if there are equal variable names, then they are likely to have max correlation. Thus this fubction set this correlations to 0.
Ac3net.filtersames <- function(mim)
{
  genes<- rownames(mim)
  ngene <- length(genes)
  for(i in 1:ngene){
    indx <- which(genes==genes[i])
    if(length(indx)>1){mim[i, indx] <- 0} 
  } 
  return(mim)
}
