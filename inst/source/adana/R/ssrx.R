# Mixed replacement function
ssrx = function(parpop, offpop, reppars, ...){
  n = nrow(parpop)
  m = ncol(parpop)
  if(nrow(offpop) > 1)
    offpop = offpop[order(offpop[,m], decreasing=TRUE),]
  parrn = rownames(parpop)
  offrn = rownames(offpop)
  bidx = which.max(parpop[,m])
  paridx = 1:n
  paridx = paridx[-c(reppars, bidx)]
  pidx = sample(paridx, size=1)
  parpop[pidx,] = offpop[1,]
  parrn[pidx] = offrn[1]
  rownames(parpop) = parrn
  return(parpop)
}