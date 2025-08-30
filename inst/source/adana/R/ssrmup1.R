# Mu+1 replacement function
ssrmup1 = function(parpop, offpop, ...){
  n = nrow(parpop)
  m = ncol(parpop)
  parrn = rownames(parpop)
  offrn = rownames(offpop)
  if(nrow(offpop) > 1)
    offpop = offpop[order(offpop[,m], decreasing=TRUE),]
  idx = sample(1:n, size=1)
  if(offpop[1,m] > parpop[idx,m]){
    parpop[idx,] = offpop[1,]
    parrn[idx] = offrn[1]
    rownames(parpop) = parrn
  }
  return(parpop)
}
