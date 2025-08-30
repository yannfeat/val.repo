# Window scaling
selwscale = function(fitvals, ns, fmin, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  fstar = rep(NA, n)
  for(i in 1:n)
    fstar[i] = fitvals[i]-fmin
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}