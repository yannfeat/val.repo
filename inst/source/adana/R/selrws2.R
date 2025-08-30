# Roulette wheel selection 2
selrws2 = function(fitvals, ns, ...){
  n = length(fitvals) # Population size
  if(missing(ns)) ns=n
  p = fitvals/sum(fitvals)
  q = cumsum(p)  # Cumulative probabilities
  matpool = rep(NA, ns) # Mating pool
  for(i in 1:ns){ 
    r = runif(1, 0, 1)
    matpool[i] = which(q >= r)[1]
  }
  return(matpool)
}