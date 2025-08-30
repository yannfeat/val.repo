# selection with stochastic fraction remainder
selrss = function(fitvals, ns, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  p = fitvals/sum(fitvals)
  expval = p*n
  intpart = floor(expval)
  rempart = expval-intpart
  matpool = c()
  for(i in 1:n)
    matpool = c(matpool, rep(i, intpart[i]))
  nfitvals = expval-intpart * sum(expval)/n
  ndif = n-length(matpool)
  remidx = selrws(nfitvals)[1:ndif]
  matpool = c(matpool, remidx)
  tmatpool = c()
  if(ns <= n){
    matpool = matpool[1:ns]
  }else{
    i=1
    repeat{
      for(j in matpool){
        tmatpool = c(tmatpool,j)
        i=i+1
        if(i>(ns-n)) break
      }
      if(i>(ns-n)) break
    }
    matpool = c(matpool, tmatpool)
  } 
  return(matpool)
}