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
Ac3net.MTC <- function(data, iterations=10, MTC=TRUE, MTCmethod="BH", estmethod='pearson')
{
    data<-as.matrix(data)
    v<-c()    #null distribution vector 
    for(i in 1:iterations)
    {
      datashufled<-matrix(sample(data), nrow(data),ncol(data)) 
      datashufled <- datashufled +1 #make sure no zero
      varofCountRows <-  apply(datashufled, 1, var) 
      i <- which(varofCountRows==0)  # because 0 var gives NA in the mim!
      if(sum(i)>0) datashufled<- datashufled[-i,]
      mim <- cor(t(datashufled), method = estmethod) #pearson or spearman if unnormalized data
      diag(mim) <-0 #no self links allowed
      mim <- Ac3net.filtersames(mim)
      #
      mim<-as.vector(mim)
      mim<-mim[mim!=0]
      v<-c(v,mim)
    } #end for i
    v <- abs(v)
    v <- sort(v)
    vl <- length(v)
    nrow_ <- nrow(mim); ncol_ <- ncol(mim)
 ########
    varofCountRows <-  apply(data, 1, var) 
    i <- which(varofCountRows==0)  # because 0 var gives NA in the mim!
    if(sum(i)>0) data<- data[-i,]
    mim <- cor(t(data), method = estmethod) #pearson or spearman if unnormalized data
    diag(mim) <-0 #no self links allowed
    mim <- Ac3net.filtersames(mim)
#########    
 m <- abs( as.vector(mim) )
 rm(mim)
 p <- rep(1,length(m))   
 maxv <- max(v)
 i <- which(m > maxv)
 if(length(i)>0) p[i] <- 0
 indx <- which( (m <= maxv) & (m != 0) )
 ln <- length(indx)
 
 for(i in 1:ln){
   ind <- which( v >= m[ indx[i] ])
   p[ indx[i] ] <- length(ind)/vl
 }    
 
 if(MTC==TRUE) p <- p.adjust(p,  method = MTCmethod)
 mimp <- matrix(p,nrow=nrow_,ncol=ncol_) 
 return(mimp) # returns a the corresponding matrix of mim with p-values
} 
  