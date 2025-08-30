# Linear ranking selection 2
sellrs2 = function(fitvals, ns, ...){
  n=length(fitvals)
  if(missing(ns)) ns=n
  fitranks = rank(fitvals, ties.method="min")
  p = 2*fitranks/(n*(n+1)) 
  matpool=sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}