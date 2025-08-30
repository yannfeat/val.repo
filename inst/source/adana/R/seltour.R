# Tournament selection
seltour = function(fitvals, ns, selt, reptype, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(ns<2) ns=2
  if(missing(selt)) selt=2
  if(selt<2 | selt>n) selt=2
  if(missing(reptype)) reptype=FALSE
  matpool = rep(NA, ns)
  for(i in 1:ns){
    tourgroup = sample(1:n, size=selt, replace=reptype)
    matpool[i] = tourgroup[which.max(fitvals[tourgroup])]
  }
  return(matpool)
}