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

#takes a network
Ac3net.getDirectedOrDualLinks <- function(net1, dual_=FALSE){
  #if directed==T then selects only the direct links from A-->B (no B-->A in this case).
  #if directed==F, then selects only the dual links A-->B and B-->A cases
  net1<- as.matrix(net1) #no levels

    net1 <- as.data.table(net1)
    net1 <- net1[!duplicated(data.table( net1[[1]],net1[[2]] ) )] # #eliminitaes multiples of A--B
    net1 <- net1[!( net1[[1]]==net1[[2]])]
    # all matrices are directionally unique now. Also no self links. 
    net1<- as.matrix(net1)
    a1<- paste0(as.character(net1[,1]),'***', as.character(net1[,2]))
    a1 <- unique(a1)
    b1 <- paste0(as.character(net1[,2]),'***', as.character(net1[,1]))
    b1 <- unique(b1)
    if(dual_==TRUE){c1 <- intersect(a1,b1); i1 <- match(c1, a1)}
    if(dual_==FALSE){c1 <- setdiff(a1,b1); i1 <- match(c1, a1)}
    return(net1[i1,])
}