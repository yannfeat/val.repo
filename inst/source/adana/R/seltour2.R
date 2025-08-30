# Tournament selection 2
seltour2 = function(fitvals, ns, selt, reptype, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(ns<2) ns=2
  if(missing(selt)) selt=2
  if(selt<2 | selt>n) selt=2
  if(missing(reptype)) reptype=FALSE
  matpool = c()
  tmppool = 1:n
  for(i in 1:ns){
    if(length(tmppool) < selt) tmppool = 1:n
    tourgroup = sample(tmppool, size=selt, replace=reptype)
    bestidx = which(fitvals==max(fitvals[tourgroup])) 
    matpool[i] = sample(bestidx, size=1)
    tmppool = tmppool[-matpool]
  }
  return(matpool)
}
