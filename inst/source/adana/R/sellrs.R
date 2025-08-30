# Linear ranking selection 1
sellrs = function(fitvals, ns, sels, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(sels)) sels=1.5 
  if(sels<1 | sels>2) return(0)
  fitranks = rank(fitvals, ties.method="min")
  fitstars = (2-sels)+2*(sels-1)*((fitranks-1)/(n-1))
  p = fitstars/sum(fitstars)
  q = cumsum(p) # Cumulative fitnees proportions
  matpool = rep(NA, ns) # Mating pool
  i = 1  # index of selected individuals
  while(i <= ns){
    r = runif(1, 0, 1)
    j = 1
    while(q[j] < r){
      j = j+1
    }
    matpool[i] = j
    i = i+1
  }
  return(matpool)
}
