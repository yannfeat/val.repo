# Boltzmann Tournament selection
selboltour = function(fitvals, ns, selt0, selg, selgmax, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selt0)) selt0=50
  if(selt0<5 | selt0>100) return(0)
  if(missing(selg) | missing(selgmax)) return(0) 
  fstar=rep(0,n)
  fmax=max(fitvals)
  for(i in 1:n){
    alfa=runif(1)
    k=1+selg/selgmax*100
    temp = selt0 * (1-alfa)^k
    fstar[i] = exp(-(fmax-fitvals[i])/temp)
  }
  p=fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}
