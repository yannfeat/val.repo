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
Ac3net.performance <- function(predictNet, referenceNet, data_, directed=TRUE)
{
  #filter reference network for the data
  genes <- rownames(data_)
  referenceNet <- as.matrix(referenceNet)
  i1 <- which(referenceNet[,1] %in% genes)
  i2 <- which(referenceNet[,2] %in% genes)
  i <- intersect(i1,i2)
  referenceNet <- referenceNet[i,]
  #
  x3 <- Ac3net.commonlinks(net1=predictNet,net2=referenceNet, directed=directed)
  TP <- nrow(x3)
  FP <- nrow(predictNet) - TP 
  FN <- nrow(referenceNet) - TP
  gnames <- unique( rownames(data_) )
  if(directed==TRUE) allx <- ( length(gnames)*length(gnames) - length(gnames) )
  if(directed==FALSE) allx <- ( length(gnames)*length(gnames) - length(gnames) )/2
  TN <- allx - (TP + FP +FN) 
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  Fscore <- 2 * precision * recall/(precision + recall)
  Accuracy <- (TP+TN)/(TP+TN+FP+FN)    #Accuracy
  output <- c(Accuracy, Fscore, precision , TP, FP, FN, TN, recall)
  names(output) <- c("Accuracy", "F-score", "precision", "TP", "FP", "FN", "TN", "recall")
  return(output)
}