# Scaling by fitness rank
selrscale = function(fitvals, ns, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  fitranks = rank(fitvals, ties.method="min")
  s = max(fitvals)/median(fitvals)
  fstar = s-2*(fitranks-1)*(s-1)/(n-1)
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}