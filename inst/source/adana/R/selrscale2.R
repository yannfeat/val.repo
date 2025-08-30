# Scaling by fitness rank 2
selrscale2 = function(fitvals, ns, sels, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(sels)) sels=1.5
  fitranks = rank(fitvals, ties.method="min")
  fstar = (2-sels)/n + (2*fitranks*(sels-1))/(n*(n-1))
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}