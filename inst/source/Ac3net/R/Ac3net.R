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
Ac3net <- function(DataOrMim, processed=FALSE, ratio_ = 0.002, PCmincutoff=0.6,PCmaxcutoff=0.96, cutoff=0,
                   estmethod='pearson', pval=1, iterations=10, MTC=FALSE, MTCmethod="BH" )
{ print("DataOrMim can be either data or adjancency matrix. 
        If you are making a comparison study and already have the adjancency matrix and
        eliminated the insignificant scores by you cutoff, then input that matrix to DataOrMim object
        and set processed=TRUE. Otherwise just enter your data along with your arbitrary parameter settings.")
  if(processed==FALSE) {
    DataOrMim <- DataOrMim +1 #make sure no zero
    varofCountRows <-  apply(DataOrMim, 1, var) 
    i <- which(varofCountRows==0)  # because 0 var gives NA in the mim!
    if(sum(i)>0) DataOrMim<- DataOrMim[-i,]
    mim <- cor(t(DataOrMim), method = estmethod) #pearson or spearman if unnormalized data
    diag(mim) <-0 #no self links allowed
    if(MTC==TRUE) {
        if(pval==1) {
          if(cutoff==0) cutoff <- Ac3net.cutoff(mim, ratio_ = ratio_, PCmincutoff=PCmincutoff, PCmaxcutoff=PCmaxcutoff)
          mim[abs(mim) < cutoff] <- 0
        }
        if(pval < 1){
          mimp <- Ac3net.MTC(data=DataOrMim, iterations=iterations, MTC=MTC, MTCmethod=MTCmethod, estmethod=estmethod)
          mim[mimp >= pval] <- 0
        }
    }else{
      if(cutoff!=0) mim[abs(mim) < cutoff] <- 0
        else{
          cutoff <- Ac3net.cutoff(mim=mim, ratio_ = ratio_, PCmincutoff=PCmincutoff, PCmaxcutoff=PCmaxcutoff)
          mim[abs(mim) < cutoff] <- 0
        } 
      
    }
    
    mim <- Ac3net.filtersames(mim)
    mim <- Ac3net.maxmim(mim) #returns Ac3net network
  }
  if(processed==TRUE){#means it is mim matrix and already eliminated by a cutoff
  # mim (DataOrMim object), must be filtered (processed) by a cutoff.
    mim <- Ac3net.filtersames(mim=DataOrMim)
    mim <- Ac3net.maxmim(mim_=mim)  #returns Ac3net network
  }
  return(mim) #returns Ac3net network
}
