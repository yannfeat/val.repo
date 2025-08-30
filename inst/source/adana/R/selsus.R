# stochastic universal selection
selsus = function(fitvals, ns, ...){
  n = length(fitvals)
  if(missing(ns)) ns = n 
  if(ns<1) return(0)
  matpool = c()
  avgfit = sum(fitvals)/ns
  firstpointer = runif(1, 0, avgfit)
  i = 0:(ns-1)
  pointers = firstpointer + i * avgfit 
  for(pointer in pointers){
    idx = 0
    while(sum(fitvals[0:idx]) < pointer)
      idx=idx+1
    matpool = c(matpool, idx)
  }
  matpool = matpool[order(runif(ns))]
  return(matpool)
}