# nonlinear ranking selection
selnlrs = function(fitvals, ns, selns, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selns)) selns=0.5
  fitranks = (n+1)-rank(fitvals, ties.method="min")
  fstar= selns*(1-selns)^(fitranks-1)
  p = pmin(pmax(0, fstar/sum(fstar)), 1)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}