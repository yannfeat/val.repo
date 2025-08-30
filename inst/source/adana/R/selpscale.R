# Power Scaling
selpscale = function(fitvals, ns, selk, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selk)) selk=1.005
  fstar = fitvals^selk
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}