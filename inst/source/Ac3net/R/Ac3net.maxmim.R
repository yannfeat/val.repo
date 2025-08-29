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
Ac3net.maxmim <- function(mim_, net_=TRUE, cutoff_=0)
{
  numprobs <- nrow(mim_)
  maxM <- c()
  for(i in 1: numprobs){
    #first eliminates the same named pairs if exist. Works in rectangular matrices as well.
    #indx <- which(colnames(mim_)==rownames(mim_)[i])
    #if(length(indx)>0) mim_[i, indx] <- 0
    mim_ <- Ac3net.filtersames(mim=mim_)
    #
    j <- which.max(abs(mim_[i,])) # compare magnitudes but not signs 
    tmp <- cbind(colnames(mim_)[j], rownames(mim_)[i], mim_[i,j], i, j) #source is in the second column
    maxM <- rbind(maxM,tmp)
  }
  colnames(maxM) <- c("Source","Target","CORR","RowIndx","ColIndx")
  if(net_==TRUE){
    maxM <- as.data.table(maxM)
    maxM <- maxM[abs(as.numeric(maxM$CORR))>cutoff_]
    maxM<- maxM[order(-abs(as.numeric(maxM$CORR)))]
    maxM$CORR <- as.numeric(maxM$CORR)
  }
  return(maxM)
}