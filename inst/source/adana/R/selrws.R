# Roulette wheel selection 1
selrws = function(fitvals, ns, ...){
  n = length(fitvals) # Population size
  if(missing(ns)) ns=n
  p = fitvals/sum(fitvals) 
  q = cumsum(p) # Cumulative probabilities
  matpool = rep(NA, ns) # Mating pool
  i = 1  # index of selected individual
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