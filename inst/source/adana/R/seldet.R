# Deterministic selection
seldet = function(fitvals, ns, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  matpool = c()
  p = fitvals/sum(fitvals) # Fitness proportions
  expval = n*p # Expected values
  intpart = floor(expval) # Integer parts
  rempart = expval-intpart # Fractional parts
  for(i in 1:n)
    matpool = c(matpool, rep(i, intpart[i])) # Selection with integer part
  emppos = n-length(matpool) # Selection with fractional part
  if(emppos!=0){
    for(i in 1:emppos){
      remidx = which.max(rempart)
      matpool = c(matpool, remidx)
      rempart = rempart[-remidx]
    }
  }
  if(ns <= n){
    matpool = matpool[1:ns]
  }else{
    tmatpool = c()
    i=1
    while(i <= (ns-n)){
      for(j in matpool){
        tmatpool[i] = j
        i=i+1
        if(length(c(matpool, tmatpool))==ns) break
      }
    }
    matpool = c(matpool, tmatpool)
  } 
  return(matpool)
}