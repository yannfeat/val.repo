#Ac3net: 
#This R package allows inferring directional conservative causal core network from large scale data.
#The inferred network consists of only direct physical interactions.
## Copyright (C) January 2011 Gokmen Altay <altaylabs@gmail.com>
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

#takes an 
Ac3net.commonlinks <- function(net1,net2, directed=TRUE){
  net1<- as.matrix(net1); net2<- as.matrix(net2) #no levels
  if(directed==TRUE){
  net1 <- as.data.table(net1); net2 <- as.data.table(net2)
  net1 <- net1[!duplicated(data.table( net1[[1]],net1[[2]] ) )] # #eliminitaes multiples of A--B
  net1 <- net1[!( net1[[1]]==net1[[2]])]
  net2 <- net2[!duplicated(data.table( net2[[1]],net2[[2]] ) )]
  net2 <- net2[!( net2[[1]]==net2[[2]])]
  # all matrices are directionally unique now. Also no self links. 
  
  net1<- as.matrix(net1); net2<- as.matrix(net2)
  a1<- paste0(as.character(net1[,1]),'***', as.character(net1[,2]))
  b1 <- paste0(as.character(net2[,1]),'***', as.character(net2[,2]))
  c1 <- intersect(a1,b1)
  i1 <- match(c1, a1)
  }
  if(directed==FALSE){
    net1 <- as.data.table(net1); net2 <- as.data.table(net2)
    net1 <- net1[!duplicated(data.table(pmin(net1[[1]],net1[[2]]),pmax(net1[[1]],net1[[2]])))]
    net2 <- net2[!duplicated(data.table(pmin(net2[[1]],net2[[2]]),pmax(net2[[1]],net2[[2]])))]
    
    net1<- as.matrix(net1); net2<- as.matrix(net2)
    a1<- paste0(as.character(net1[,1]),'***', as.character(net1[,2]))
    b1 <- paste0(as.character(net2[,1]),'***', as.character(net2[,2]))
    c1 <- intersect(a1,b1)
    i1 <- match(c1, a1)
    
    b2 <- paste0(as.character(net2[,2]),'***', as.character(net2[,1]))
    c2 <- intersect(a1,b2)
    i2 <- match(c2, a1)
    i1 <- union(i1,i2)
  }
  commonnet <- net1[i1,]
  return(commonnet)
}