# Genitor replacement function
ssrgenitor = function(parpop, offpop, ...){
  n = nrow(parpop)
  m = ncol(parpop)
  parrn = rownames(parpop)
  offrn = rownames(offpop)
  parpop = parpop[order(parpop[,m], decreasing=TRUE),]
  if(nrow(offpop) > 1)
    offpop = offpop[order(offpop[,m], decreasing=TRUE),]
  parpop[n,] = offpop[1,]
  parrn[n] = offrn[1]
  rownames(parpop) = parrn
  return(parpop)
}
