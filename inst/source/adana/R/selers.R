# Exponantial ranking selection
selers = function(fitvals, ns, selbc, ...){
  n = length(fitvals)
  if(missing(selbc)) selbc=0.5
  if(missing(ns)) ns=n
  fitranks = rank(fitvals, ties.method="min")
  p = selbc^(n-fitranks) / sum(selbc^(n-fitranks))
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}