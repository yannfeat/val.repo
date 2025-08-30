# Truncation Selection
seltrunc = function(fitvals, ns, selps, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selps)) selps=0.5
  if(selps<=0 | selps>1) return(0) 
  ts = ifelse(round(n*selps)<2, 2, round(n*selps))
  matpool = c()
  names(fitvals) = 1:n
  sfitvals = sort(fitvals, decreasing=TRUE)
  if(ts>=ns){
    matpool[1:ns] = as.integer(names(sfitvals)[1:ns])
  }else{
    matpool[1:ts] = as.integer(names(sfitvals)[1:ts])
    for(i in 1: (ns-ts)){
      for(j in matpool){
        if(length(matpool)==ns) break
        matpool = c(matpool, j)
      }
    }
  }
  return(matpool)
}	